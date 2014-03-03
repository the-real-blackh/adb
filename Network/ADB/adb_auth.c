#define _HASKELL_HOOKS_

#if defined(TEXT_BASE)
#define U_BOOT
#endif

#if !defined(U_BOOT)
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#else
#include <common.h>
#include <malloc.h>
#endif

#include "adb_auth.h"


#define RSANUMBYTES 256           /* 2048 bit key length */
#define RSANUMWORDS (RSANUMBYTES / sizeof(uint32_t))

typedef struct RSAPublicKey {
    int len;                  /* Length of n[] in number of uint32_t */
    uint32_t n0inv;           /* -1 / n[0] mod 2^32 */
    uint32_t n[RSANUMWORDS];  /* modulus as little endian array */
    uint32_t rr[RSANUMWORDS]; /* R^2 as little endian array */
    int exponent;             /* 3 or 65537 */
} RSAPublicKey;

#include "adb_auth_private_key.h"

void adb_auth_init(ADBAuth* auth)
{
    auth->ctx = NULL;
    RSA_priv_key_new(&auth->ctx,
        public_n, public_n_length,
        public_e, public_e_length,
        private_d, private_d_length);
    auth->key_try_ix = 0;
}

#if defined(_HASKELL_HOOKS_)
ADBAuth* adb_auth_new(void)
{
    ADBAuth* auth = (ADBAuth*)malloc(sizeof(ADBAuth));
    adb_auth_init(auth);
    return auth;
}
#endif

void adb_auth_free(ADBAuth* auth)
{
    RSA_free(auth->ctx);
}

static char encoding_table[] = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
                                'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
                                'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                                'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
                                'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
                                'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                                'w', 'x', 'y', 'z', '0', '1', '2', '3',
                                '4', '5', '6', '7', '8', '9', '+', '/'};
static int mod_table[] = {0, 2, 1};

void base64_encode(const unsigned char *data,
                    size_t input_length,
                    char* encoded_data) {

    int output_length = 4 * ((input_length + 2) / 3);
    int i, j;

    for (i = 0, j = 0; i < input_length;) {

        uint32_t octet_a = i < input_length ? data[i++] : 0;
        uint32_t octet_b = i < input_length ? data[i++] : 0;
        uint32_t octet_c = i < input_length ? data[i++] : 0;

        uint32_t triple = (octet_a << 0x10) + (octet_b << 0x08) + octet_c;

        encoded_data[j++] = encoding_table[(triple >> 3 * 6) & 0x3F];
        encoded_data[j++] = encoding_table[(triple >> 2 * 6) & 0x3F];
        encoded_data[j++] = encoding_table[(triple >> 1 * 6) & 0x3F];
        encoded_data[j++] = encoding_table[(triple >> 0 * 6) & 0x3F];
    }

    for (i = 0; i < mod_table[input_length % 3]; i++)
        encoded_data[output_length - 1 - i] = '=';
    encoded_data[output_length] = 0;
}

static const uint8_t prefix[] = {
  0x30,0x21,0x30,0x09,0x06,0x05,0x2b,0x0e,0x03,0x02,0x1a,0x05,0x00,0x04,0x14
};

/* Handle adb's AUTH command. If it wants to reply, it will return
 * with 1 and pkt will contain the packet to be sent. */
int adb_auth(ADBAuth* auth, apacket* pkt)
{
    switch (pkt->msg.arg0) {
    case ADB_AUTH_TOKEN:
        if (auth->key_try_ix == 0) {
            uint8_t digest[4096];
            uint8_t answer[MAX_PAYLOAD];
            int outLen;
            memcpy(digest, prefix, sizeof(prefix));
            memcpy(digest+sizeof(prefix), pkt->data, pkt->msg.data_length);
            outLen = RSA_encrypt(auth->ctx, digest, sizeof(prefix)+pkt->msg.data_length, answer, 1);
            memcpy(pkt->data, answer, outLen);
            pkt->msg.command = A_AUTH;
            pkt->msg.arg0 = ADB_AUTH_SIGNATURE;
            pkt->msg.arg1 = 0;
            pkt->msg.data_length = (unsigned) outLen;
            auth->key_try_ix = 1;
            return 1;
        }
        else {
            base64_encode((const unsigned char *) &public_key, sizeof(public_key), (char*)pkt->data);
            pkt->msg.command = A_AUTH;
            pkt->msg.arg0 = ADB_AUTH_RSAPUBLICKEY;
            pkt->msg.arg1 = 0;
            pkt->msg.data_length = (unsigned) strlen((const char*)pkt->data) + 1;
            return 1;
        }
    case ADB_AUTH_SIGNATURE:
        return 0;
    case ADB_AUTH_RSAPUBLICKEY:
        return 0;
    default:
        return 0;
    }
}


#ifndef _ADB_AUTH_H_
#define _ADB_AUTH_H_

#include "adb.h"
#if !defined(U_BOOT)
#include <stdint.h>
#endif
#include "crypto.h"

typedef struct {
    RSA_CTX* ctx;
    int key_try_ix;
} ADBAuth;

void adb_auth_init(ADBAuth* auth);
#if defined(_HASKELL_HOOKS_)
ADBAuth* adb_auth_new(void);
#endif
void adb_auth_free(ADBAuth* auth);

/* Handle adb's AUTH command. If it wants to reply, it will return
 * with 1 and pkt will contain the packet to be sent. */
int adb_auth(ADBAuth* auth, apacket* pkt);

#endif


#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32

#define IOTATIME_EXPORT __declspec(dllexport)

IOTATIME_EXPORT void *iotatime_unix_locale_snapshot(const char *name) {
    (void)name;
    return NULL;
}

IOTATIME_EXPORT const char *iotatime_unix_locale_item(void *snapshot,
                                                       int index) {
    (void)snapshot;
    (void)index;
    return "";
}

IOTATIME_EXPORT void iotatime_unix_locale_free(void *snapshot) {
    (void)snapshot;
}

#else

#include <langinfo.h>
#include <locale.h>

#define IOTATIME_EXPORT
#define IOTATIME_LOCALE_ITEM_COUNT 43

typedef struct {
    char *items[IOTATIME_LOCALE_ITEM_COUNT];
} IotaTimeLocaleSnapshot;

static const nl_item locale_items[IOTATIME_LOCALE_ITEM_COUNT] = {
    MON_1, MON_2, MON_3, MON_4, MON_5, MON_6,
    MON_7, MON_8, MON_9, MON_10, MON_11, MON_12,
    ABMON_1, ABMON_2, ABMON_3, ABMON_4, ABMON_5, ABMON_6,
    ABMON_7, ABMON_8, ABMON_9, ABMON_10, ABMON_11, ABMON_12,
    DAY_1, DAY_2, DAY_3, DAY_4, DAY_5, DAY_6, DAY_7,
    ABDAY_1, ABDAY_2, ABDAY_3, ABDAY_4, ABDAY_5, ABDAY_6, ABDAY_7,
    AM_STR, PM_STR, D_FMT, T_FMT, D_T_FMT
};

static void free_snapshot(IotaTimeLocaleSnapshot *snapshot) {
    int index;

    if (snapshot == NULL) {
        return;
    }
    for (index = 0; index < IOTATIME_LOCALE_ITEM_COUNT; ++index) {
        free(snapshot->items[index]);
    }
    free(snapshot);
}

IOTATIME_EXPORT void *iotatime_unix_locale_snapshot(const char *name) {
    IotaTimeLocaleSnapshot *snapshot;
    locale_t locale;
    int index;

    locale = newlocale(LC_ALL_MASK, name, (locale_t)0);
    if (locale == (locale_t)0) {
        return NULL;
    }
    snapshot = (IotaTimeLocaleSnapshot *)calloc(1, sizeof(*snapshot));
    if (snapshot == NULL) {
        freelocale(locale);
        return NULL;
    }
    for (index = 0; index < IOTATIME_LOCALE_ITEM_COUNT; ++index) {
        const char *value = nl_langinfo_l(locale_items[index], locale);
        snapshot->items[index] = strdup(value == NULL ? "" : value);
        if (snapshot->items[index] == NULL) {
            freelocale(locale);
            free_snapshot(snapshot);
            return NULL;
        }
    }
    freelocale(locale);
    return snapshot;
}

IOTATIME_EXPORT const char *iotatime_unix_locale_item(void *value, int index) {
    IotaTimeLocaleSnapshot *snapshot = (IotaTimeLocaleSnapshot *)value;

    if (snapshot == NULL || index < 0 || index >= IOTATIME_LOCALE_ITEM_COUNT) {
        return "";
    }
    return snapshot->items[index];
}

IOTATIME_EXPORT void iotatime_unix_locale_free(void *snapshot) {
    free_snapshot((IotaTimeLocaleSnapshot *)snapshot);
}

#endif
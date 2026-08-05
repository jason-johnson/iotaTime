#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define IOTATIME_EXPORT __declspec(dllexport)

typedef struct {
    char *data;
    size_t length;
    size_t capacity;
    int failed;
} StringBuilder;

#define IOTATIME_WINDOWS_LOCALE_ITEM_COUNT 43

typedef struct {
    char *items[IOTATIME_WINDOWS_LOCALE_ITEM_COUNT];
} WindowsLocaleSnapshot;

typedef int32_t (*IcuGetWindowsTimeZoneId)(const uint16_t *, int32_t,
                                           uint16_t *, int32_t, int32_t *);
typedef int32_t (*IcuGetTimeZoneIdForWindowsId)(const uint16_t *, int32_t,
                                                const char *, uint16_t *,
                                                int32_t, int32_t *);

typedef struct {
    HMODULE module;
    IcuGetWindowsTimeZoneId iana_to_windows;
    IcuGetTimeZoneIdForWindowsId windows_to_iana;
} IcuZoneFunctions;

static INIT_ONCE icu_zone_once = INIT_ONCE_STATIC_INIT;
static IcuZoneFunctions icu_zone_functions = {0};

static char *copy_string(const char *value) {
    size_t length = strlen(value);
    char *copy = (char *)malloc(length + 1);

    if (copy != NULL) {
        memcpy(copy, value, length + 1);
    }
    return copy;
}

static char *wide_to_utf8(const WCHAR *value) {
    int required = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, value, -1,
                                       NULL, 0, NULL, NULL);
    char *result;

    if (required <= 0) {
        return NULL;
    }
    result = (char *)malloc((size_t)required);
    if (result == NULL) {
        return NULL;
    }
    if (WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, value, -1, result,
                            required, NULL, NULL) <= 0) {
        free(result);
        return NULL;
    }
    return result;
}

static WCHAR *utf8_to_wide(const char *value) {
    int required = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, value, -1,
                                       NULL, 0);
    WCHAR *result;

    if (required <= 0) {
        return NULL;
    }
    result = (WCHAR *)malloc((size_t)required * sizeof(WCHAR));
    if (result == NULL) {
        return NULL;
    }
    if (MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, value, -1, result,
                            required) <= 0) {
        free(result);
        return NULL;
    }
    return result;
}

static BOOL CALLBACK initialize_icu_zone_functions(PINIT_ONCE once,
                                                    PVOID parameter,
                                                    PVOID *context) {
    HMODULE module;

    (void)once;
    (void)parameter;
    (void)context;
    module = LoadLibraryW(L"icu.dll");
    if (module == NULL) {
        return TRUE;
    }
    icu_zone_functions.iana_to_windows = (IcuGetWindowsTimeZoneId)
        GetProcAddress(module, "ucal_getWindowsTimeZoneID");
    icu_zone_functions.windows_to_iana = (IcuGetTimeZoneIdForWindowsId)
        GetProcAddress(module, "ucal_getTimeZoneIDForWindowsID");
    if (icu_zone_functions.iana_to_windows == NULL ||
        icu_zone_functions.windows_to_iana == NULL) {
        icu_zone_functions.iana_to_windows = NULL;
        icu_zone_functions.windows_to_iana = NULL;
        FreeLibrary(module);
        return TRUE;
    }
    icu_zone_functions.module = module;
    return TRUE;
}

static void ensure_icu_zone_functions(void) {
    InitOnceExecuteOnce(&icu_zone_once, initialize_icu_zone_functions,
                        NULL, NULL);
}

IOTATIME_EXPORT void *iotatime_windows_iana_to_windows(const char *iana_id) {
    WCHAR *input;
    uint16_t output[128];
    int32_t status = 0;
    int32_t length;

    ensure_icu_zone_functions();
    if (icu_zone_functions.iana_to_windows == NULL) {
        return NULL;
    }
    input = utf8_to_wide(iana_id);
    if (input == NULL) {
        return NULL;
    }
    length = icu_zone_functions.iana_to_windows((const uint16_t *)input, -1,
                                                 output, 128, &status);
    free(input);
    if (status > 0 || length <= 0 || length >= 128) {
        return NULL;
    }
    output[length] = 0;
    return wide_to_utf8((const WCHAR *)output);
}

IOTATIME_EXPORT void *iotatime_windows_windows_to_iana(const char *windows_id) {
    WCHAR *input;
    uint16_t output[128];
    int32_t status = 0;
    int32_t length;

    ensure_icu_zone_functions();
    if (icu_zone_functions.windows_to_iana == NULL) {
        return NULL;
    }
    input = utf8_to_wide(windows_id);
    if (input == NULL) {
        return NULL;
    }
    length = icu_zone_functions.windows_to_iana((const uint16_t *)input, -1,
                                                 "001", output, 128, &status);
    free(input);
    if (status > 0 || length <= 0 || length >= 128) {
        return NULL;
    }
    output[length] = 0;
    return wide_to_utf8((const WCHAR *)output);
}

static char *locale_info_utf8(LPCWSTR locale_name, LCTYPE type) {
    int required = GetLocaleInfoEx(locale_name, type, NULL, 0);
    WCHAR *wide;
    char *result;

    if (required <= 0) {
        return NULL;
    }
    wide = (WCHAR *)malloc((size_t)required * sizeof(WCHAR));
    if (wide == NULL) {
        return NULL;
    }
    if (GetLocaleInfoEx(locale_name, type, wide, required) <= 0) {
        free(wide);
        return NULL;
    }
    result = wide_to_utf8(wide);
    free(wide);
    return result;
}

static void free_locale_snapshot(WindowsLocaleSnapshot *snapshot) {
    int index;

    if (snapshot == NULL) {
        return;
    }
    for (index = 0; index < IOTATIME_WINDOWS_LOCALE_ITEM_COUNT; ++index) {
        free(snapshot->items[index]);
    }
    free(snapshot);
}

static void builder_reserve(StringBuilder *builder, size_t extra) {
    size_t required;
    size_t capacity;
    char *resized;

    if (builder->failed) {
        return;
    }
    required = builder->length + extra + 1;
    if (required <= builder->capacity) {
        return;
    }
    capacity = builder->capacity == 0 ? 4096 : builder->capacity;
    while (capacity < required) {
        capacity *= 2;
    }
    resized = (char *)realloc(builder->data, capacity);
    if (resized == NULL) {
        builder->failed = 1;
        return;
    }
    builder->data = resized;
    builder->capacity = capacity;
}

static void builder_append_bytes(StringBuilder *builder, const char *value,
                                 size_t length) {
    builder_reserve(builder, length);
    if (builder->failed) {
        return;
    }
    memcpy(builder->data + builder->length, value, length);
    builder->length += length;
    builder->data[builder->length] = '\0';
}

static void builder_append(StringBuilder *builder, const char *value) {
    builder_append_bytes(builder, value, strlen(value));
}

static void builder_append_wide(StringBuilder *builder, const WCHAR *value) {
    int required;

    required = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, value, -1,
                                   NULL, 0, NULL, NULL);
    if (required <= 0) {
        builder->failed = 1;
        return;
    }
    builder_reserve(builder, (size_t)required - 1);
    if (builder->failed) {
        return;
    }
    if (WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, value, -1,
                            builder->data + builder->length, required,
                            NULL, NULL) <= 0) {
        builder->failed = 1;
        return;
    }
    builder->length += (size_t)required - 1;
}

static void builder_append_hex(StringBuilder *builder, const BYTE *value,
                               DWORD length) {
    static const char digits[] = "0123456789ABCDEF";
    DWORD index;

    builder_reserve(builder, (size_t)length * 2);
    if (builder->failed) {
        return;
    }
    for (index = 0; index < length; ++index) {
        builder->data[builder->length++] = digits[value[index] >> 4];
        builder->data[builder->length++] = digits[value[index] & 0x0f];
    }
    builder->data[builder->length] = '\0';
}

static LONG read_string(HKEY key, const WCHAR *name, WCHAR **value) {
    DWORD type = 0;
    DWORD size = 0;
    LONG status;
    WCHAR *buffer;

    status = RegQueryValueExW(key, name, NULL, &type, NULL, &size);
    if (status != ERROR_SUCCESS) {
        return status;
    }
    if (type != REG_SZ && type != REG_EXPAND_SZ) {
        return ERROR_INVALID_DATATYPE;
    }
    buffer = (WCHAR *)malloc((size_t)size + sizeof(WCHAR));
    if (buffer == NULL) {
        return ERROR_OUTOFMEMORY;
    }
    status = RegQueryValueExW(key, name, NULL, &type, (BYTE *)buffer, &size);
    if (status != ERROR_SUCCESS) {
        free(buffer);
        return status;
    }
    buffer[size / sizeof(WCHAR)] = L'\0';
    *value = buffer;
    return ERROR_SUCCESS;
}

static LONG read_binary(HKEY key, const WCHAR *name, BYTE **value,
                        DWORD *length) {
    DWORD type = 0;
    DWORD size = 0;
    LONG status;
    BYTE *buffer;

    status = RegQueryValueExW(key, name, NULL, &type, NULL, &size);
    if (status != ERROR_SUCCESS) {
        return status;
    }
    if (type != REG_BINARY) {
        return ERROR_INVALID_DATATYPE;
    }
    buffer = (BYTE *)malloc(size == 0 ? 1 : size);
    if (buffer == NULL) {
        return ERROR_OUTOFMEMORY;
    }
    status = RegQueryValueExW(key, name, NULL, &type, buffer, &size);
    if (status != ERROR_SUCCESS) {
        free(buffer);
        return status;
    }
    *value = buffer;
    *length = size;
    return ERROR_SUCCESS;
}

static LONG read_dword(HKEY key, const WCHAR *name, DWORD *value) {
    DWORD type = 0;
    DWORD size = sizeof(*value);
    LONG status = RegQueryValueExW(key, name, NULL, &type, (BYTE *)value, &size);

    if (status == ERROR_SUCCESS && type != REG_DWORD) {
        return ERROR_INVALID_DATATYPE;
    }
    return status;
}

static char *registry_error(const char *operation, LONG status) {
    char buffer[256];
    int length = snprintf(buffer, sizeof(buffer),
                          "ERROR\t%s failed with Windows status %ld",
                          operation, (long)status);
    char *result;

    if (length < 0) {
        return NULL;
    }
    result = (char *)malloc((size_t)length + 1);
    if (result != NULL) {
        memcpy(result, buffer, (size_t)length + 1);
    }
    return result;
}

static LONG append_dynamic_rules(StringBuilder *builder, HKEY zone_key) {
    HKEY dynamic_key;
    LONG status;
    DWORD first_year;
    DWORD last_year;
    DWORD year;

    status = RegOpenKeyExW(zone_key, L"Dynamic DST", 0, KEY_READ, &dynamic_key);
    if (status == ERROR_FILE_NOT_FOUND) {
        return ERROR_SUCCESS;
    }
    if (status != ERROR_SUCCESS) {
        return status;
    }
    status = read_dword(dynamic_key, L"FirstEntry", &first_year);
    if (status == ERROR_SUCCESS) {
        status = read_dword(dynamic_key, L"LastEntry", &last_year);
    }
    if (status != ERROR_SUCCESS || first_year > last_year) {
        RegCloseKey(dynamic_key);
        return status == ERROR_SUCCESS ? ERROR_INVALID_DATA : status;
    }

    for (year = first_year; year <= last_year; ++year) {
        WCHAR name[16];
        BYTE *tzi = NULL;
        DWORD tzi_length = 0;

        _snwprintf_s(name, 16, _TRUNCATE, L"%lu", (unsigned long)year);
        status = read_binary(dynamic_key, name, &tzi, &tzi_length);
        if (status != ERROR_SUCCESS) {
            RegCloseKey(dynamic_key);
            return status;
        }
        builder_append(builder, "DYNAMIC\t");
        {
            char year_buffer[16];
            snprintf(year_buffer, sizeof(year_buffer), "%lu", (unsigned long)year);
            builder_append(builder, year_buffer);
        }
        builder_append(builder, "\t");
        builder_append_hex(builder, tzi, tzi_length);
        builder_append(builder, "\n");
        free(tzi);
        if (builder->failed || year == UINT32_MAX) {
            break;
        }
    }
    RegCloseKey(dynamic_key);
    return builder->failed ? ERROR_OUTOFMEMORY : ERROR_SUCCESS;
}

static LONG append_zone(StringBuilder *builder, HKEY zones_key,
                        const WCHAR *zone_id) {
    HKEY zone_key;
    WCHAR *standard_name = NULL;
    WCHAR *daylight_name = NULL;
    BYTE *tzi = NULL;
    DWORD tzi_length = 0;
    LONG status;

    status = RegOpenKeyExW(zones_key, zone_id, 0, KEY_READ, &zone_key);
    if (status != ERROR_SUCCESS) {
        return status;
    }
    status = read_string(zone_key, L"Std", &standard_name);
    if (status == ERROR_SUCCESS) {
        status = read_string(zone_key, L"Dlt", &daylight_name);
    }
    if (status == ERROR_SUCCESS) {
        status = read_binary(zone_key, L"TZI", &tzi, &tzi_length);
    }
    if (status == ERROR_SUCCESS) {
        builder_append(builder, "ZONE\nID\t");
        builder_append_wide(builder, zone_id);
        builder_append(builder, "\nSTD\t");
        builder_append_wide(builder, standard_name);
        builder_append(builder, "\nDST\t");
        builder_append_wide(builder, daylight_name);
        builder_append(builder, "\nTZI\t");
        builder_append_hex(builder, tzi, tzi_length);
        builder_append(builder, "\n");
        status = builder->failed ? ERROR_OUTOFMEMORY
                                 : append_dynamic_rules(builder, zone_key);
    }
    if (status == ERROR_SUCCESS) {
        builder_append(builder, "END\n");
        if (builder->failed) {
            status = ERROR_OUTOFMEMORY;
        }
    }

    free(standard_name);
    free(daylight_name);
    free(tzi);
    RegCloseKey(zone_key);
    return status;
}

IOTATIME_EXPORT void *iotatime_windows_registry_snapshot(void) {
    static const WCHAR zones_path[] =
        L"SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones";
    static const WCHAR local_path[] =
        L"SYSTEM\\CurrentControlSet\\Control\\TimeZoneInformation";
    StringBuilder builder = {0};
    HKEY zones_key = NULL;
    HKEY local_key = NULL;
    WCHAR *local_zone = NULL;
    LONG status;
    DWORD index = 0;

    status = RegOpenKeyExW(HKEY_LOCAL_MACHINE, local_path, 0,
                           KEY_READ | KEY_WOW64_64KEY, &local_key);
    if (status == ERROR_SUCCESS) {
        status = read_string(local_key, L"TimeZoneKeyName", &local_zone);
        RegCloseKey(local_key);
    }
    if (status != ERROR_SUCCESS) {
        return registry_error("reading the local time zone", status);
    }

    builder_append(&builder, "LOCAL\t");
    builder_append_wide(&builder, local_zone);
    builder_append(&builder, "\n");
    free(local_zone);
    if (builder.failed) {
        free(builder.data);
        return registry_error("allocating the registry snapshot", ERROR_OUTOFMEMORY);
    }

    status = RegOpenKeyExW(HKEY_LOCAL_MACHINE, zones_path, 0,
                           KEY_READ | KEY_WOW64_64KEY, &zones_key);
    if (status != ERROR_SUCCESS) {
        free(builder.data);
        return registry_error("opening Windows time zones", status);
    }

    for (;;) {
        WCHAR zone_id[256];
        DWORD zone_id_length = 256;
        FILETIME last_write;

        status = RegEnumKeyExW(zones_key, index, zone_id, &zone_id_length,
                               NULL, NULL, NULL, &last_write);
        if (status == ERROR_NO_MORE_ITEMS) {
            status = ERROR_SUCCESS;
            break;
        }
        if (status != ERROR_SUCCESS) {
            break;
        }
        zone_id[zone_id_length] = L'\0';
        status = append_zone(&builder, zones_key, zone_id);
        if (status != ERROR_SUCCESS) {
            break;
        }
        ++index;
    }
    RegCloseKey(zones_key);

    if (status != ERROR_SUCCESS) {
        free(builder.data);
        return registry_error("enumerating Windows time zones", status);
    }
    return builder.data;
}

IOTATIME_EXPORT void *iotatime_windows_locale_snapshot(const char *name,
                                                        int current) {
    WindowsLocaleSnapshot *snapshot;
    WCHAR *wide_name = NULL;
    LPCWSTR locale_name;
    int index;

    if (current) {
        locale_name = LOCALE_NAME_USER_DEFAULT;
    } else if (strcmp(name, "C") == 0 || strcmp(name, "POSIX") == 0 ||
               name[0] == '\0') {
        locale_name = LOCALE_NAME_INVARIANT;
    } else {
        wide_name = utf8_to_wide(name);
        if (wide_name == NULL) {
            return NULL;
        }
        locale_name = wide_name;
    }
    if (GetLocaleInfoEx(locale_name, LOCALE_SMONTHNAME1, NULL, 0) <= 0) {
        free(wide_name);
        return NULL;
    }
    snapshot = (WindowsLocaleSnapshot *)calloc(1, sizeof(*snapshot));
    if (snapshot == NULL) {
        free(wide_name);
        return NULL;
    }
    snapshot->items[0] = current
        ? locale_info_utf8(locale_name, LOCALE_SNAME)
        : copy_string(name);
    for (index = 0; index < 12; ++index) {
        snapshot->items[1 + index] = locale_info_utf8(
            locale_name, LOCALE_SMONTHNAME1 + (LCTYPE)index);
        snapshot->items[13 + index] = locale_info_utf8(
            locale_name, LOCALE_SABBREVMONTHNAME1 + (LCTYPE)index);
    }
    snapshot->items[25] = locale_info_utf8(locale_name, LOCALE_SDAYNAME7);
    snapshot->items[32] = locale_info_utf8(locale_name,
                                            LOCALE_SABBREVDAYNAME7);
    for (index = 0; index < 6; ++index) {
        snapshot->items[26 + index] = locale_info_utf8(
            locale_name, LOCALE_SDAYNAME1 + (LCTYPE)index);
        snapshot->items[33 + index] = locale_info_utf8(
            locale_name, LOCALE_SABBREVDAYNAME1 + (LCTYPE)index);
    }
    snapshot->items[39] = locale_info_utf8(locale_name, LOCALE_S1159);
    snapshot->items[40] = locale_info_utf8(locale_name, LOCALE_S2359);
    snapshot->items[41] = locale_info_utf8(locale_name, LOCALE_SSHORTDATE);
    snapshot->items[42] = locale_info_utf8(locale_name, LOCALE_STIMEFORMAT);
    free(wide_name);

    for (index = 0; index < IOTATIME_WINDOWS_LOCALE_ITEM_COUNT; ++index) {
        if (snapshot->items[index] == NULL) {
            free_locale_snapshot(snapshot);
            return NULL;
        }
    }
    return snapshot;
}

IOTATIME_EXPORT const char *iotatime_windows_locale_item(void *value,
                                                          int index) {
    WindowsLocaleSnapshot *snapshot = (WindowsLocaleSnapshot *)value;

    if (snapshot == NULL || index < 0 ||
        index >= IOTATIME_WINDOWS_LOCALE_ITEM_COUNT) {
        return "";
    }
    return snapshot->items[index];
}

IOTATIME_EXPORT void iotatime_windows_locale_free(void *snapshot) {
    free_locale_snapshot((WindowsLocaleSnapshot *)snapshot);
}

#else

#define IOTATIME_EXPORT

IOTATIME_EXPORT void *iotatime_windows_registry_snapshot(void) {
    static const char message[] =
        "ERROR\tnative Windows registry access is unavailable on this platform";
    char *result = (char *)malloc(sizeof(message));
    if (result != NULL) {
        memcpy(result, message, sizeof(message));
    }
    return result;
}

IOTATIME_EXPORT void *iotatime_windows_iana_to_windows(const char *iana_id) {
    (void)iana_id;
    return NULL;
}

IOTATIME_EXPORT void *iotatime_windows_windows_to_iana(const char *windows_id) {
    (void)windows_id;
    return NULL;
}

IOTATIME_EXPORT void *iotatime_windows_locale_snapshot(const char *name,
                                                        int current) {
    (void)name;
    (void)current;
    return NULL;
}

IOTATIME_EXPORT const char *iotatime_windows_locale_item(void *snapshot,
                                                          int index) {
    (void)snapshot;
    (void)index;
    return "";
}

IOTATIME_EXPORT void iotatime_windows_locale_free(void *snapshot) {
    (void)snapshot;
}

#endif

IOTATIME_EXPORT const char *iotatime_windows_snapshot_string(void *snapshot) {
    return (const char *)snapshot;
}

IOTATIME_EXPORT void iotatime_windows_snapshot_free(void *snapshot) {
    free(snapshot);
}

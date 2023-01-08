:warning: This is currently not supported.

# fuse_adb_fs
FUSE module for accessing Android devices over ADB

This is development tool, it is not intended to be used as regular file
transfer tool. The reason is that it currently uses temporary files on
sdcard for transferring data. A lot of copying can reduce lifetime of flash
card or device memory.

This allows at now:

1. Have multiple devices be visible in one fs
2. Copy files from device
3. Copy files to device
4. Edit files in place on device (block read/write available)
5. Create directories
6. Delete directories

Features that hopefully will be added in future:

1. Screenshot
2. Applications list

Build:

1. You'll need an [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/) being installed.
2. Clone fuse_adb_fs repo
3. In the repo root, execute:

```
stack build
```

The built executable will be placed in `.stack-work/dist/x86_64-linux-tinfo6/Cabal-1.24.0.0/build/fuse-adb-fs/fuse-adb-fs`

Usage:

```
fuse-adb-fs <mountpoint>
```

Also, for debugging purposes, log file can be created:

```
fuse-adb-fs -f /tmp/adb_fs_log.txt <mountpoint>
```


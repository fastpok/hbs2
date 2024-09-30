# Quickstart

1.  ```shell
    cd examples/chat
    ```

2.  ```shell
    cabal run chat
    ```

3.  Open [localhost:3000](http://localhost:3000/) in your browser.

## Create chat reflog

1. Create file containing refchan head

   ```
   (version 1)
   (quorum 2)
   (wait 10)

   (peer "<peer-key-1>" 1)
   (peer "<peer-key-2>" 1)

   (author "<user-1-sign-key>")
   (author "<user-2-sign-key>")

   (reader "<user-1-encryption-key>")
   (reader "<user-2-encryption-key>")
   ```

2. ```shell
   hbs2-cli [hbs2:refchan:create test-refchan-head.txt]
   ```

   This command will print reflog key, save it somwhere, we'll need it later. It will also create relog key in hbs2-keyman folder.

## Create sigil

Make sure that you have a keyring file with at least 1 encryption key.

```shell
hbs2 keyring-new -n 1 > /path-to-keyring.key
```

```shell
hbs2-cli hbs2:sigil:create-from-keyring 1 [str:read-file /path-to-keyring.key] > sigil.txt
```

or

```shell
hbs2 sigil create -k /path-to-keyring.key <encryption-key-from-keyring> > sigil.txt
```

# Quickstart

1. Сlone the hbs2 repository

   ```shell
    git clone hbs2://BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP hbs2
   ```

2. Go to the hbs2 directory

   ```shell
   cd hbs2
   ```

3. Activate build environment

   ```shell
   nix develop
   ```

4. Create config file at `~/.config/hbs2-chat/config` with the following contents

   ```
   sigil "sigil.txt"
   refchan "<refchan-id>" "chat-name"
   ```

   If you dont have sigils and refchans read the followng sections:

   [Create chat refchan](#create-chat-refchan)

   [Create sigil](#create-sigil)

5. Run chat server

   ```shell
   cabal run chat
   ```

6. Open [localhost:3000](http://localhost:3000/) in your browser.

## Create chat refchan

1. Create file containing refchan head

   ```
   (version 1)
   (quorum 1)
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

   This command will print refchan key, save it somwhere, we'll need it later. It will also create refchan key in hbs2-keyman folder.

3. Make sure that refchan is ready:
   ```
   hbs2-peer refchan get <refchan-key>
   ```

> Note:
> command `hbs2-peer refchan get <refchan-key>` and chat app may not work on empty refchans. So you can write some message to refchan using CLI.

```shell
hbs2-peer refchan propose -a <author-key> -f some-file.txt <refchan-key>
```

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

## Update refchan head

Edit refchan head file or create a new one.

> Note:
> don't forget to increase refchan head version.

```shell
hbs2-cli hbs2:refchan:head:update <refchan-key> new-refchan-head.txt
```

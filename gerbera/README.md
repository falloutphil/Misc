# Gerbera on a QNAP NAS for a Roberts Revival iStream 3

A complete, reproducible QNAP Container Station setup with read-only source media, persistent Gerbera state, host-network UPnP discovery, and an adaptive FLAC/DSF compatibility transcoder.

> **Status:** tested with **Gerbera 3.2.1** using the Docker image `gerbera/gerbera:3.2.1-transcoding` and a **Roberts Revival iStream 3**.
>
> **Important:** this is an **unofficial community setup**. It is not supplied, endorsed, or supported by Roberts Radio, QNAP, Gerbera, or MinimServer. Product and project names are the property of their respective owners.

---

## Contents

1. [What this project solves](#what-this-project-solves)
2. [Why Gerbera](#why-gerbera)
3. [Architecture](#architecture)
4. [Security model](#security-model)
5. [What you need](#what-you-need)
6. [Repository files](#repository-files)
7. [Step 1 - install Container Station](#step-1---install-container-station)
8. [Step 2 - enable SSH temporarily](#step-2---enable-ssh-temporarily)
9. [Step 3 - choose host paths and create persistent storage](#step-3---choose-host-paths-and-create-persistent-storage)
10. [Step 4 - give the container read-only access to the music tree](#step-4---give-the-container-read-only-access-to-the-music-tree)
11. [Step 5 - create the Gerbera container in Container Station](#step-5---create-the-gerbera-container-in-container-station)
12. [Step 6 - verify the first Gerbera start](#step-6---verify-the-first-gerbera-start)
13. [Step 7 - install `config.xml` and the Roberts transcoder](#step-7---install-configxml-and-the-roberts-transcoder)
14. [Step 8 - understand what must be changed in `config.xml`](#step-8---understand-what-must-be-changed-in-configxml)
15. [Step 9 - start Gerbera and run the self-check](#step-9---start-gerbera-and-run-the-self-check)
16. [Step 10 - add the music folder as an autoscan directory](#step-10---add-the-music-folder-as-an-autoscan-directory)
17. [Step 11 - prove the source library is read-only](#step-11---prove-the-source-library-is-read-only)
18. [Step 12 - test playback](#step-12---test-playback)
19. [How the adaptive transcoder works](#how-the-adaptive-transcoder-works)
20. [Why a sustained-load model is better than a single peak](#why-a-sustained-load-model-is-better-than-a-single-peak)
21. [AWK mini-tutorial](#awk-mini-tutorial)
22. [The packet-metrics AWK algorithm in detail](#the-packet-metrics-awk-algorithm-in-detail)
23. [Adaptive candidate calibration](#adaptive-candidate-calibration)
24. [Effective-bit quantisation](#effective-bit-quantisation)
25. [Sample-rate policy](#sample-rate-policy)
26. [DSF/SACD policy](#dsfsacd-policy)
27. [Cache design and invalidation](#cache-design-and-invalidation)
28. [Why the script uses `exec`](#why-the-script-uses-exec)
29. [Troubleshooting](#troubleshooting)
30. [Updating or recreating the container](#updating-or-recreating-the-container)
31. [Public-GitHub safety checklist](#public-github-safety-checklist)
32. [References](#references)

---

# What this project solves

The Roberts Revival iStream 3 is a good network radio, but it is much less forgiving of some high-resolution lossless files than a modern computer, phone, or dedicated hi-fi streamer.

In this project the important practical observation was that **encoded FLAC load matters, not just sample rate and bit depth**. Ordinary CD-quality FLAC normally plays natively. Some 24-bit material also plays natively. Other files spend enough time at a high encoded bitrate that the radio pauses with a `Loading...` indication.

A simple rule such as “convert every 24-bit file” throws away quality unnecessarily. A rule such as “only convert files above 48 kHz” also misses difficult 24/48 FLACs. The useful question is therefore:

> **Can this particular file be delivered to this renderer at a sustained encoded rate it can actually consume?**

The solution in this repository keeps the source library untouched and lets Gerbera call a wrapper script which decides, per file, whether to:

- stream the original FLAC byte-for-byte;
- resample only when the renderer cannot accept the source rate;
- reduce the **effective** PCM precision only as far as necessary;
- downmix multichannel material to stereo;
- convert DSF to a predictable FLAC compatibility stream;
- cache the decision so later plays start quickly.

The result is intentionally not a universal audio-transcoding recipe. It is a renderer-compatibility policy derived from real measurements on this class of Roberts radio.

---

# Why Gerbera

## QNAP's built-in media serving

The QNAP media stack is perfectly adequate for many normal DLNA/UPnP jobs, but this project needed a programmable decision layer in front of the renderer: inspect a file, estimate sustained transport stress, optionally perform a short calibration encode, and then choose a live output format without touching the original.

That is beyond the job of a conventional “share this media library” server.

## Earlier MinimServer / MinimStreamer attempt

MinimServer remains an excellent music-library server. In the tested setup, MinimStreamer was able to express useful format/sample-rate rules, for example converting high-rate FLAC to renderer-compatible PCM.

It fell short **for this particular problem** because the decision needed to depend on the **actual encoded FLAC load**, not merely source format, nominal bit depth, or source sample rate. The tested local-transcoding path also did not provide the adaptive FLAC-in to FLAC-out workflow required here. During multichannel DSF testing, the external downmix path also exposed a channel-metadata mismatch in this environment.

Those are use-case limitations, not a general criticism of MinimServer.

## Why Gerbera fitted

Gerbera's external transcoding model is unusually useful here:

1. Gerbera receives a UPnP request.
2. It launches a configured external transcoder.
3. `%in` becomes the source file path.
4. `%out` becomes Gerbera's output FIFO.
5. A wrapper script can run arbitrary logic before choosing what to stream.
6. FFmpeg writes the live FLAC stream to the FIFO.
7. Gerbera sends it to the renderer.

The official Gerbera Docker project also provides a `-transcoding` image containing FFmpeg and other tools, so the transcoding environment is reproducible instead of depending on whatever old multimedia binaries happen to be bundled with the NAS firmware.

---

# Architecture

```text
                    home LAN
                       |
          +------------+-------------+
          |                          |
+---------v---------+       +--------v----------------+
| Roberts iStream 3 |       | QNAP NAS               |
| UPnP renderer     |       |                        |
+-------------------+       | Container Station      |
                            |   Gerbera container     |
                            |   host networking       |
                            |   TCP 49494 / SSDP      |
                            |          |              |
                            |          v              |
                            |  roberts-audio.sh       |
                            |   ffprobe + AWK         |
                            |   ffmpeg live FLAC      |
                            |          |              |
                            |          v              |
                            |  /mnt/content  [RO]     |
                            |          ^              |
                            |          | bind mount   |
                            |  NAS music library      |
                            |                         |
                            | /var/run/gerbera [RW]   |
                            | config + DB + cache     |
                            +-------------------------+
```

Two host bind mounts are the key:

| NAS host path | Container path | Access | Purpose |
|---|---|---:|---|
| your existing music directory | `/mnt/content` | **Read only** | source media |
| a dedicated Gerbera state directory | `/var/run/gerbera` | **Read/write** | config, SQLite database, cache and wrapper script |

The container uses **host networking** because UPnP discovery depends on multicast. Gerbera's own Docker documentation specifically recommends host networking as the simplest way to avoid the multicast limitations of the default Docker bridge.

---

# Security model

This setup deliberately uses several independent controls.

## 1. Source media is a Docker read-only bind mount

Even if the wrapper script, FFmpeg, or Gerbera tries to write into `/mnt/content`, Docker rejects the write at the mount boundary.

This is stronger than merely relying on application behaviour.

## 2. Gerbera does not run privileged

Do **not** enable QNAP Container Station's **Privileged mode** for this container.

The official Gerbera image normally runs as the `gerbera` user. The tested 3.2.1 image uses UID/GID **1042** by default.

## 3. The music tree can grant only named read/traverse ACLs to UID 1042

There is no need to make the music world-readable, change the existing owner, or add Gerbera to a root-equivalent group.

## 4. Persistent writable state is isolated

Only the dedicated Gerbera state directory needs to be writable by UID 1042.

## 5. Do not expose the Gerbera web UI to the Internet

The UI includes a filesystem browser and is intended for a trusted home LAN. **Do not create router/NAT port-forwarding rules for TCP 49494.** Do not expose SSDP/UPnP ports externally either.

The example config leaves Gerbera UI accounts disabled for LAN convenience. If the LAN itself is not trusted, enable appropriate protection or disable the web UI after setup.

---

# What you need

- A QNAP NAS supported by Container Station.
- QTS / QuTS hero with Container Station installed or installable.
- A working home LAN.
- A Roberts Revival iStream 3 on that LAN.
- An existing music directory on the NAS.
- An administrator account for the QNAP.
- The repository files:
  - `config.example.xml`
  - `roberts-audio-v4.1.sh`
  - this `README.md`
- Enough NAS CPU to encode FLAC faster than real time. The measured system had a very large margin; your NAS should be tested rather than assumed.

This guide deliberately pins:

```text
gerbera/gerbera:3.2.1-transcoding
```

That is the tested baseline. Do not replace a pinned production setup with `latest` merely because a newer tag exists. Upgrade deliberately and retest.

---

# Repository files

Recommended public repository layout:

```text
.
├── README.md
├── config.example.xml
├── roberts-audio-v4.1.sh
├── .gitignore
└── LICENSE
```

The supplied `config.example.xml` must contain only placeholders, for example:

```xml
<account user="gerbera-change-me" password="gerbera-change-me" />
<udn>uuid:my-long-uuid-change-me</udn>
```

Do **not** put a live Gerbera UDN, real credentials, NAS IP address, private media path, database, cache, or runtime logs into a public repository.

---

# Step 1 - install Container Station

QNAP's current Container Station 3 workflow is organised around the left-hand navigation menu. Minor wording can vary between QTS and Container Station point releases, but the layout below corresponds to the current Container Station 3 family and the tested setup.

## Through QTS

1. Sign in to the QNAP QTS web interface as an administrator.
2. Open **App Center** from the QTS desktop.
3. Use the App Center search box and search for **Container Station**.
4. Click **Install** if it is not already installed.
5. When installation completes, click **Open**.
6. Allow Container Station to complete any first-run initialisation it requests.

Container Station's main interface has:

- a toolbar across the top;
- a navigation menu on the left;
- the selected resource page in the main panel.

We will use **Containers** from the left menu.

---

# Step 2 - enable SSH temporarily

The container itself can be created in the GUI, but the safest way to set exact ownership, ACLs and executable permissions is through SSH.

In QTS:

1. Open **Control Panel**.
2. Open **Network & File Services**.
3. Open **Telnet / SSH** (sometimes displayed as `Telnet/SSH`).
4. Tick **Allow SSH connection**.
5. Leave the port at your normal SSH port, commonly `22`, unless you already use another port.
6. Click **Apply**.

Only an administrator-authorised account should be used.

From another computer, connect with your preferred SSH client:

```bash
ssh YOUR_QNAP_ADMIN_USER@YOUR_NAS_LAN_ADDRESS
```

`YOUR_QNAP_ADMIN_USER` and `YOUR_NAS_LAN_ADDRESS` are deliberately placeholders. Never put the real values into a public README.

If you do not normally use SSH, you can disable **Allow SSH connection** again after installation and testing.

---

# Step 3 - choose host paths and create persistent storage

The examples below use placeholders.

```text
MUSIC_HOST_PATH=/share/YourMusicShare/Music
GERBERA_HOST_PATH=/share/Container/gerbera
```

Replace only `MUSIC_HOST_PATH` with the real directory containing your music.

The Gerbera state path can normally remain under QNAP's `Container` shared folder.

## Check the music path

```bash
MUSIC_HOST_PATH='/share/YourMusicShare/Music'
ls -ld "$MUSIC_HOST_PATH"
```

Do not continue until that resolves to the correct library.

## Create the persistent Gerbera directory

```bash
GERBERA_HOST_PATH='/share/Container/gerbera'

sudo mkdir -p "$GERBERA_HOST_PATH"
sudo chown -v 1042:1042 "$GERBERA_HOST_PATH"
sudo chmod -v 770 "$GERBERA_HOST_PATH"
```

Why UID/GID `1042`? The tested official Gerbera image uses UID/GID 1042 for its `gerbera` user. Gerbera's official Docker documentation also exposes `UID` and `GID` environment overrides if you deliberately want a different mapping.

For this guide, keeping the image default makes the setup predictable.

---

# Step 4 - give the container read-only access to the music tree

There are **two separate permission layers** here:

1. the NAS filesystem must allow UID 1042 to read/traverse the files;
2. Docker then mounts the entire tree **read-only** inside the container.

The second layer prevents writes even though the first grants normal filesystem read access.

## Back up ACL information first

Before changing ACLs, save a record outside the music tree:

```bash
MUSIC_HOST_PATH='/share/YourMusicShare/Music'
GERBERA_HOST_PATH='/share/Container/gerbera'

sudo getfacl -R "$MUSIC_HOST_PATH" \
  > "$GERBERA_HOST_PATH/music-acl-before.txt"
```

Keep this backup private. It may contain real directory and file names, so **do not commit it to GitHub**.

## Grant UID 1042 read/traverse access

Directories need `r-x`; files need `r--`.

```bash
sudo find "$MUSIC_HOST_PATH" -type d \
  -exec setfacl -m u:1042:rx '{}' \;

sudo find "$MUSIC_HOST_PATH" -type f \
  -exec setfacl -m u:1042:r-- '{}' \;
```

To allow new subdirectories/files created later to inherit an ACL entry for UID 1042, add a default ACL on directories:

```bash
sudo find "$MUSIC_HOST_PATH" -type d \
  -exec setfacl -m d:u:1042:rx '{}' \;
```

This approach preserves existing owner/group entries instead of taking ownership of the library.

> **Do not solve this by putting the Gerbera process in group 0/root.** That grants much more authority than a media server needs.

If your QNAP is configured with a different permission model or `setfacl` is unavailable, use the NAS's supported ACL mechanism or deliberately remap the container UID/GID. The principle remains the same: **least privilege plus a read-only bind mount**.

---

# Step 5 - create the Gerbera container in Container Station

This is the GUI path in detail.

## 5.1 Open the Create Container wizard

1. Open **Container Station**.
2. In the **left sidebar**, click **Containers**.
3. At the **top of the Containers page**, click **Create**.
4. The **Create Container** window opens.

## 5.2 Select the image

In the image-selection part of the wizard:

1. Select **Basic Mode** if the wizard asks for a mode.
2. Select **Docker Hub** as the registry.
3. In the image field enter:

   ```text
   gerbera/gerbera:3.2.1-transcoding
   ```

4. If shown, tick **Try pulling the image from the registry before creating the container**.
5. Click **Next**.

The `-transcoding` suffix matters: it is the Gerbera image variant that includes FFmpeg and other transcoding tools.

## 5.3 Basic container settings

Set:

```text
Name: gerbera
```

For automatic restart/start behaviour select:

```text
Unless Stopped
```

Some Container Station versions describe a closely equivalent policy as **Retain previous state**. The desired behaviour is: start automatically after NAS/Container Station restart unless you deliberately stopped the container.

Do not click Finish yet.

## 5.4 Open Advanced Settings

Click **Advanced Settings**.

Container Station opens a settings panel/window with tabs such as:

- Commands
- Networks
- Environments
- Labels
- Storage
- Runtime
- Resources

The critical tabs are **Networks**, **Storage**, and **Runtime**.

## 5.5 Networks tab - select Host mode

1. Click **Networks** in the Advanced Settings list.
2. Find **Network Mode** / **Network mode**.
3. Select:

   ```text
   Host
   ```

4. Do **not** add normal Docker port-forwarding rules for 49494 or 1900 when using host mode.

Why Host mode?

UPnP discovery uses SSDP multicast. Gerbera's own Docker documentation warns that the default Docker bridge setup does not support the required multicast behaviour cleanly and recommends host networking as the easiest solution.

Gerbera then listens directly on the NAS network stack, normally using:

```text
TCP 49494   Gerbera HTTP / web UI / media serving
UDP 1900    SSDP discovery
```

Make sure TCP 49494 is not already occupied by another service on the NAS.

## 5.6 Storage tab - add the read-only music bind mount

1. Click **Storage** in Advanced Settings.
2. Click **Add Volume**.
3. Choose **Bind Mount Host Path**.
4. For the **Host path**, browse to or enter your music directory, for example:

   ```text
   /share/YourMusicShare/Music
   ```

5. For the **Container path / Mount point**, enter:

   ```text
   /mnt/content
   ```

6. Set the mount permission to:

   ```text
   Read Only / RO
   ```

This is the most important source-media safety setting in the whole deployment.

## 5.7 Storage tab - add persistent Gerbera state

Still in **Storage**:

1. Click **Add Volume** again.
2. Choose **Bind Mount Host Path**.
3. Host path:

   ```text
   /share/Container/gerbera
   ```

4. Container path:

   ```text
   /var/run/gerbera
   ```

5. Permission:

   ```text
   Read/Write / RW
   ```

This is where the container keeps persistent state including:

- `config.xml`
- `gerbera.db`
- database backups
- `roberts-audio.sh`
- `roberts-cache-v4/`

Because this is a host bind mount, recreating the container does not throw those files away.

## 5.8 Runtime tab

1. Click **Runtime**.
2. Leave the normal Docker runtime as **runc**.
3. Ensure **Privileged mode** is **OFF**.
4. Do not add host devices unless you have a separate reason unrelated to this guide.

Host networking is not supported by QNAP's Kata runtime, so use the normal Docker runtime here.

## 5.9 Environment, Commands and Resources

For the tested image:

- **Commands:** no custom command required.
- **Entrypoint:** leave default.
- **Environment:** no additional variables required when using the image's default UID/GID 1042.
- **Resources:** defaults are normally fine initially. Avoid an artificially tiny CPU limit because live transcoding needs CPU headroom.

## 5.10 Review and create

1. Click **Apply** or return from Advanced Settings, depending on your Container Station version.
2. Click **Next**.
3. Review the summary carefully.
4. Verify **Host** network mode.
5. Verify `/mnt/content` is **RO**.
6. Verify `/var/run/gerbera` is **RW**.
7. Verify **Privileged mode is off**.
8. Click **Finish**.

Container Station pulls the image and creates the container.

## Docker CLI equivalent

You do not need to use this if you created the container in the GUI. It is included as an audit/reference representation of the same important settings:

```bash
docker run -d \
  --name gerbera \
  --restart unless-stopped \
  --network host \
  -v /share/YourMusicShare/Music:/mnt/content:ro \
  -v /share/Container/gerbera:/var/run/gerbera:rw \
  gerbera/gerbera:3.2.1-transcoding
```

---

# Step 6 - verify the first Gerbera start

SSH to the NAS and run:

```bash
docker logs --tail 100 gerbera
```

A successful start should eventually report that Gerbera has bound to the NAS and provide the web UI address on port 49494.

Open from a browser on the same trusted LAN:

```text
http://YOUR_NAS_LAN_ADDRESS:49494
```

Do not forward this port from your Internet router.

At this point an empty persistent directory may contain a generated `config.xml` and SQLite database. We will now install the tested configuration and wrapper.

---

# Step 7 - install `config.xml` and the Roberts transcoder

Assume you have copied the repository files to a temporary NAS location, for example QNAP's `Public` share:

```text
/share/Public/config.example.xml
/share/Public/roberts-audio-v4.1.sh
```

Using `/share/Public` here is only an example staging location. Remove the temporary copies later if you do not want them there.

## 7.1 Stop Gerbera

```bash
docker stop gerbera
```

## 7.2 Back up any generated config

If a generated config already exists:

```bash
sudo cp -av \
  /share/Container/gerbera/config.xml \
  /share/Container/gerbera/config.xml.generated-backup
```

## 7.3 Copy the public-safe example config into place

```bash
sudo cp -v \
  /share/Public/config.example.xml \
  /share/Container/gerbera/config.xml
```

## 7.4 Generate a real unique UDN

The public example deliberately contains:

```xml
<udn>uuid:my-long-uuid-change-me</udn>
```

Every running UPnP server instance should have its own stable unique identifier.

Generate one on the NAS:

```bash
cat /proc/sys/kernel/random/uuid
```

Suppose that prints:

```text
YOUR-NEW-UNIQUE-UUID
```

Edit `/share/Container/gerbera/config.xml` and change:

```xml
<udn>uuid:my-long-uuid-change-me</udn>
```

to:

```xml
<udn>uuid:YOUR-NEW-UNIQUE-UUID</udn>
```

Do not publish that live UDN in your public repository.

You may do the replacement non-interactively:

```bash
UUID="$(cat /proc/sys/kernel/random/uuid)"

sudo sed -i \
  "s|uuid:my-long-uuid-change-me|uuid:${UUID}|" \
  /share/Container/gerbera/config.xml
```

Then verify:

```bash
grep -n '<udn>' /share/Container/gerbera/config.xml
```

## 7.5 Copy the wrapper script

```bash
sudo cp -v \
  /share/Public/roberts-audio-v4.1.sh \
  /share/Container/gerbera/roberts-audio.sh
```

The name inside the persistent directory is intentionally `roberts-audio.sh` because that is the path referenced by `config.xml`.

## 7.6 Set ownership and modes

```bash
sudo chown -v 1042:1042 \
  /share/Container/gerbera/config.xml \
  /share/Container/gerbera/roberts-audio.sh

sudo chmod -v 660 \
  /share/Container/gerbera/config.xml

sudo chmod -v 755 \
  /share/Container/gerbera/roberts-audio.sh
```

The script must be executable.

---

# Step 8 - understand what must be changed in `config.xml`

The provided `config.example.xml` already contains the transcoding profile used by this project. You should understand the few site-specific fields before running it.

## 8.1 UDN - must be unique

Change:

```xml
<udn>uuid:my-long-uuid-change-me</udn>
```

to a generated UUID as described above.

## 8.2 UI account placeholders

The public example contains deliberately fake values such as:

```xml
<accounts enabled="no" session-timeout="30">
  <account user="gerbera-change-me" password="gerbera-change-me" />
</accounts>
```

Because `enabled="no"`, these are not active credentials.

If you decide to enable Gerbera UI accounts, replace them with real local values and **do not commit the modified live config to GitHub**.

Gerbera's own documentation warns that its UI authentication is not a substitute for placing the server on a trusted network. The best security boundary is still network exposure: LAN only, no Internet port forwarding.

## 8.3 Persistent home

The config uses:

```xml
<home>/var/run/gerbera/</home>
```

That matches the writable host bind mount we created.

## 8.4 SQLite database

The config uses a local SQLite database:

```xml
<sqlite3 enabled="yes">
  <database-file>gerbera.db</database-file>
  <backup enabled="yes" interval="600" />
</sqlite3>
```

Because `home` is `/var/run/gerbera/`, the database persists in the NAS bind-mounted state directory.

The MySQL/PostgreSQL examples remain disabled and contain public-safe placeholders.

## 8.5 FLAC mapping

The important mappings are:

```xml
<transcode mimetype="audio/flac" using="roberts-smart-flac" />
<transcode mimetype="audio/x-flac" using="roberts-smart-flac" />
```

The profile calls:

```xml
<agent command="/var/run/gerbera/roberts-audio.sh"
       arguments="flac %in %out" />
```

Gerbera substitutes `%in` and `%out` at playback time.

## 8.6 DSF mapping

The DSF mapping is:

```xml
<transcode mimetype="audio/x-dsf" using="roberts-dsf-flac" />
```

and the profile calls:

```xml
<agent command="/var/run/gerbera/roberts-audio.sh"
       arguments="dsf %in %out" />
```

## 8.7 First resource

The profiles use:

```xml
<first-resource>yes</first-resource>
```

Some renderers inspect only the first resource Gerbera advertises for a track. This setting puts the compatibility resource first while leaving the original resource visible.

## 8.8 Output buffering

The Roberts profiles use:

```xml
<buffer size="4194304"
        chunk-size="131072"
        fill-size="262144"
        timeout="10"
        retry-count="4" />
```

This is Gerbera's output-side buffering between the external transcoder and renderer. It is distinct from the Roberts radio's own internal buffering.

---

# Step 9 - start Gerbera and run the self-check

Start the container:

```bash
docker start gerbera
sleep 3
```

Check startup:

```bash
docker logs --tail 80 gerbera
```

You want configuration load and validation to succeed.

Now execute the wrapper's diagnostic mode **as the actual Gerbera user**, not as Docker root:

```bash
docker exec gerbera sh -c '
su-exec gerbera /var/run/gerbera/roberts-audio.sh check
'
```

You should see paths for tools including:

```text
ffmpeg
ffprobe
awk
stat
cat
mv
mkdir
cksum
head
id
```

and, most importantly:

```text
cache-dir    writable: /var/run/gerbera/roberts-cache-v4 (uid=1042)
```

## Why the `su-exec gerbera` part matters

`docker exec` defaults to root. A root-only test can give a false sense of security and can even create a cache directory owned by root, preventing the real Gerbera process from writing later.

Version 4.1 of the wrapper contains extra protection against this mistake, but the correct test is still to run it as `gerbera`.

If the cache directory has wrong ownership, fix it on the NAS:

```bash
sudo chown -Rv 1042:1042 \
  /share/Container/gerbera/roberts-cache-v4

sudo chmod -v 700 \
  /share/Container/gerbera/roberts-cache-v4
```

Then rerun the self-check.

---

# Step 10 - add the music folder as an autoscan directory

Gerbera can define autoscan directories in XML or through the web UI. This setup uses the UI so the scan entry is stored in Gerbera's persistent database/config state.

Open:

```text
http://YOUR_NAS_LAN_ADDRESS:49494
```

Then:

1. In Gerbera's **left menu**, click **Filesystem**.
2. Browse the filesystem tree to:

   ```text
   /mnt/content
   ```

3. Select `/mnt/content` in the tree.
4. In the trail/breadcrumb operations for the selected directory, choose **Add Autoscan Directory**.
5. Set the scan mode to **Inotify**.
6. Enable **Recursive**.
7. Leave **Hidden files** disabled.
8. Leave symlink following disabled unless you have a deliberate reason to enable it.
9. If the dialog offers media-type filtering, select **Music/Audio** for a music-only library.
10. Save/apply the autoscan entry.

Gerbera should begin importing the tree.

To confirm:

1. Click **Database** in the left menu.
2. Open **PC Directory** or one of the virtual music views such as Albums/Artists.
3. Confirm that tracks appear.

With Inotify enabled, new files are normally imported when filesystem events occur. A full scan also occurs as required by Gerbera's autoscan logic.

---

# Step 11 - prove the source library is read-only

Do not merely assume that selecting RO in the GUI worked. Test it.

Run:

```bash
docker exec gerbera sh -c '
su-exec gerbera touch /mnt/content/GERBERA-WRITE-TEST
'
```

The expected result is an error similar to:

```text
Read-only file system
```

That failure is **success**: it proves the container cannot modify the source bind mount.

If the command unexpectedly succeeds, immediately remove the test file from the NAS and inspect the Container Station storage settings. The music bind must be recreated or edited as **Read Only** before trusting the setup.

You can also test that reading works by selecting a known file and using `test -r` or `ffprobe` inside the container.

---

# Step 12 - test playback

A sensible progression is:

1. normal 16-bit/44.1-kHz CD FLAC;
2. low-bitrate 24/44.1 FLAC;
3. low-bitrate 24/48 FLAC;
4. a difficult 24/48 FLAC;
5. 24/88.2 FLAC;
6. 24/96 FLAC;
7. stereo DSF;
8. multichannel DSF.

Watch live Gerbera logs while testing:

```bash
docker logs -f --since 10s gerbera
```

## Expected behaviour

### Safe ordinary FLAC

A sufficiently safe in-envelope file should show a native pass decision and stream byte-for-byte.

### Difficult FLAC on first play

A difficult file may show:

```text
SOURCE ... cache=miss
FAST DECISION -> transcode ...
SAMPLED SOURCE ...
CALIBRATE ...
CANDIDATE ...
CALIBRATE selected ...
TRANSCODE -> FLAC ...
```

A first play may therefore take around a second or a little more to begin on a difficult uncached track, depending on NAS CPU, storage and file characteristics.

### Same difficult FLAC on later play

The same unchanged file should become:

```text
CACHE HIT -> FLAC ...
TRANSCODE -> FLAC ... cached decision
```

with no new sample/calibration work.

---

# How the adaptive transcoder works

The wrapper is designed around a simple principle:

> **Do not spend CPU or startup time proving something that is already obvious.**

The decision pipeline is therefore deliberately asymmetric.

## Stage 0 - cache first

Before `ffprobe` is launched, the wrapper computes an identity for the source file and looks for a cached decision.

A valid hit immediately executes one of the stored actions:

- `pass`
- `effective`
- `reencode24`
- `compat24`
- `s16`

This makes repeat playback cheap.

## Stage 1 - one quick metadata probe

On a cache miss, one `ffprobe` call reads:

- sample rate;
- channel count;
- raw bit depth where available;
- duration;
- file size;
- format bitrate where available.

If the container bitrate is absent, the script estimates the mean encoded rate as:

```text
mean bits/s = file_size_bytes * 8 / duration_seconds
```

This does not decode the whole track.

## Stage 2 - renderer envelope

The source is considered natively “in envelope” only if it is:

```text
stereo
sample rate <= 48 kHz
bit depth <= 24 bits
```

Files outside that envelope need compatibility conversion even if their FLAC compression happens to be excellent.

## Stage 3 - obvious safe fast path

If a supported FLAC has mean encoded rate at or below:

```text
1,300,000 bit/s
```

it passes immediately.

The 1.3-Mbit/s threshold gives enough headroom that further packet analysis is normally a waste of startup time.

## Stage 4 - obvious hot fast path

If the mean is at or above:

```text
1,500,000 bit/s
```

there is no value in spending time trying to prove that a finite receiver buffer can tolerate it indefinitely. The source is treated as needing intervention.

## Stage 5 - grey band

The interesting range is approximately:

```text
1.3 Mbit/s < mean < 1.5 Mbit/s
```

A track with that mean could be:

- mostly easy with a handful of tall bursts; or
- persistently difficult with long dense regions.

So the script samples packet load across the track.

## Stage 6 - short calibration only when conversion is required

Once conversion is necessary, the script does **not** encode the whole file to find the answer.

It finds the hardest sampled region and encodes only a short 12-second calibration segment at candidate effective bit depths.

The first candidate is an estimate. The script then walks upward or downward as necessary, with a hard maximum number of attempts.

Once a candidate passes the sustained-load criteria, that decision is cached and the real live stream begins.

---

# Why a sustained-load model is better than a single peak

Lossless codecs are variable-bitrate. Two seconds of music at the same PCM sample rate and bit depth can produce very different FLAC packet sizes.

A “maximum packet bitrate” rule sounds safe but is often the wrong model for a buffered network renderer.

Suppose the receiver can consume data at a roughly steady rate and has some buffer space.

A very short burst above that rate can be absorbed if:

1. the buffer had spare capacity before the burst; and
2. later lower-rate data allows the buffer to recover.

What causes trouble is sustained positive pressure.

The wrapper therefore measures several complementary quantities.

## Mean

Long-term load:

```text
sum(packet bits) / sum(packet duration)
```

If the mean itself is above the sustainable consumption rate for long enough, a finite buffer must eventually lose.

## P95 and P99

These describe the high end of the ordinary distribution without letting one freak packet dominate the decision.

- **P95:** approximately 95% of measured one-second buckets are at or below this rate.
- **P99:** approximately 99% are at or below this rate.

## Max 1-second bucket

Useful diagnostically, but deliberately not an automatic veto.

## Max rolling 3-second average

This asks a more useful question than “what was the tallest instant?”:

> What was the worst short **sustained** load across three consecutive seconds?

## Fraction of seconds above 1.5 Mbit/s

One second over the line in a ten-minute track is not the same as five minutes continuously over the line.

## Longest consecutive run above 1.5 Mbit/s

This distinguishes isolated spikes from a genuinely hot region.

## Comparative buffer debt

The script also computes a simple leaky-buffer style quantity:

```text
debt += (rate - LIMIT) / 8
if debt < 0: debt = 0
```

Because each bucket is one second, `rate - LIMIT` is surplus bits for that second; dividing by 8 converts it to bytes.

When the bitrate is below the limit, debt drains. When it is above, debt grows. The maximum observed debt is a useful comparative indicator of how much temporary storage a constant-rate consumer would have needed over that sampled run.

It is **not** claimed to be the Roberts radio's real buffer size.

---

# AWK mini-tutorial

The script intentionally uses AWK for the packet mathematics instead of shell loops, Python, Perl, or `jq`.

Why?

- AWK is tiny and normally present in a Linux container.
- It is implemented as a compiled program and is very fast for line-oriented numeric reduction.
- It can process a stream in one pass without storing giant shell arrays.
- It starts quickly, which matters on the track-start path.

## Basic AWK shape

An AWK program normally looks like:

```awk
BEGIN {
    # Run once before input.
}

{
    # Run once for every input record/line.
}

END {
    # Run once after all input has been consumed.
}
```

AWK automatically reads standard input one line at a time.

## Fields

If input is:

```text
one|two|three
```

and AWK is started with:

```bash
awk -F'|'
```

then:

```awk
$1 == "one"
$2 == "two"
$3 == "three"
NF == 3
```

`NF` means “number of fields”.

## Variables

AWK variables do not need declarations:

```awk
count++
total += value
```

## Associative arrays

AWK arrays can use numeric or string keys:

```awk
bytes[123] += 4096
hist[150]++
```

This is ideal for:

- one-second time buckets;
- histogram bins.

## `split()`

Given:

```text
size=12345
```

this:

```awk
split($f, kv, "=")
```

creates:

```text
kv[1] = "size"
kv[2] = "12345"
```

## Passing shell values into AWK

The wrapper calls AWK with values such as:

```bash
awk -v LIMIT="$LIMIT_BPS" -v BIN="$HIST_BIN_BPS" '...'
```

Inside AWK, `LIMIT` and `BIN` are immediately available as numeric variables.

## Numeric conversion

AWK automatically treats numeric-looking strings as numbers. The script sometimes uses:

```awk
value + 0
```

to make the intended numeric interpretation explicit.

## Integer-like bucketing

```awk
sec = int(t)
```

turns a packet timestamp such as `123.742` into one-second bucket `123`.

Similarly:

```awk
bin = int(rate / BIN)
```

places a rate into a histogram bin.

## Why AWK instead of sorting all rates

An exact percentile could store every one-second rate and sort them. That is unnecessary here.

The script uses 10-kbit/s histogram bins. The histogram is tiny, and an approximate P95/P99 at 10-kbit/s resolution is more than adequate for a renderer whose engineering threshold is around 1.5 Mbit/s.

That means:

- no external `sort` process;
- no large array of textual numbers;
- predictable CPU and memory cost.

---

# The packet-metrics AWK algorithm in detail

FFprobe is asked for only three packet properties:

```text
pts_time
duration_time
size
```

and formats each packet as compact key/value fields separated by `|`.

Conceptually, input looks like:

```text
pts_time=10.240000|duration_time=0.096000|size=16384
pts_time=10.336000|duration_time=0.096000|size=15872
...
```

The function `packet_metrics_awk()` processes that stream.

## 1. Parse each packet

For every input record:

```awk
t = 0
d = 0
s = 0

for (f = 1; f <= NF; f++) {
    split($f, kv, "=")
    if (kv[1] == "pts_time")      t = kv[2] + 0
    else if (kv[1] == "duration_time") d = kv[2] + 0
    else if (kv[1] == "size")    s = kv[2] + 0
}
```

The code deliberately does not depend on field order. It looks at each key.

If a packet has no useful duration or size it is skipped.

## 2. Assign packets to one-second buckets

```awk
sec = int(t)
bytes[sec] += s
present[sec] = 1
```

All packets whose PTS is within the same integer second contribute to the same bucket.

The script also accumulates:

```awk
totalbytes += s
totaldur   += d
```

for a duration-weighted mean over the inspected packets.

## 3. Build a histogram

For every present second:

```awk
rate = bytes[i] * 8
bin  = int(rate / BIN)
hist[bin]++
```

`BIN` is 10,000 bit/s.

A 1,447,000-bit/s second therefore lands in approximately bin 144.

## 4. Count seconds over the engineering limit

```awk
if (rate > LIMIT) {
    over++
    run++
    if (run > maxrun) maxrun = run
} else {
    run = 0
}
```

This gives both:

- total number of hot seconds;
- longest consecutive hot run.

## 5. Reset runs across sampling gaps

The grey-band analysis does not inspect the whole file. It seeks to several short windows.

It would be mathematically wrong to treat the end of one sampled window and the start of another, perhaps twenty minutes later, as consecutive seconds.

So the code checks:

```awk
if (i != prev + 1) {
    run  = 0
    debt = 0
}
```

This is subtle but important.

Sampling gaps are **unknown audio**, not zero-bitrate audio and not contiguous audio.

## 6. Compute comparative buffer debt

For a one-second bucket:

```awk
debt += (rate - LIMIT) / 8
if (debt < 0) debt = 0
if (debt > maxdebt) maxdebt = debt
```

Example with a 1.5-Mbit/s limit:

```text
second A: 1.70 Mbit/s -> +25,000 bytes debt
second B: 1.60 Mbit/s -> +12,500 bytes debt
second C: 1.20 Mbit/s -> -37,500 bytes debt -> returns to 0
```

The model says the burst could be absorbed by roughly 37.5 KiB of temporary capacity under this simplified constant-consumer assumption.

Again, it is a **comparative engineering metric**, not a reverse-engineered hardware buffer size.

## 7. Compute the worst rolling 3-second average

For every fully contiguous three-second group:

```awk
v = (r[i] + r[i+1] + r[i+2]) / 3
```

The maximum is retained along with its starting second.

That `worst3` location later becomes the anchor for candidate calibration.

## 8. Approximate P95/P99 from the histogram

If there are `n` buckets:

```awk
t95 = int(n * 0.95 + 0.999999)
t99 = int(n * 0.99 + 0.999999)
```

The `+0.999999` is a compact positive-number ceiling technique.

Then the histogram is walked from low to high:

```awk
cum += hist[b]
```

The first bin whose cumulative count reaches the target becomes the percentile estimate.

The code reports the **upper edge** of the bin:

```awk
p95 = (b + 1) * BIN
p99 = (b + 1) * BIN
```

So the estimate is intentionally conservative by up to one 10-kbit/s bin.

## 9. Emit shell assignments

Instead of JSON, the AWK code emits a small trusted numeric line such as:

```text
M_OK=1 M_AVG=1447000 M_P95=1560000 M_P99=1560000 ...
```

The shell evaluates those assignments.

This is safe in this context because the values are generated internally as numeric values by AWK, not copied from arbitrary media tags or file names.

---

# How the sampling windows are chosen

For a normal long track the wrapper uses:

```text
9 windows x 5 seconds = about 45 seconds inspected
```

The windows are distributed approximately evenly from the start to the end of the file.

For duration `d`, window width `w`, and `n` windows:

```text
usable = d - w
start_i = int(usable * i / (n - 1))
```

for:

```text
i = 0 ... n-1
```

FFprobe receives intervals in the form:

```text
0%+5,1375%+5,2750%+5,...
```

for a sufficiently long example file.

The exact values depend on duration.

The key property is **bounded work**: a four-hour mix does not cause a four-hour packet scan.

If a track is shorter than the total nominal sample budget, the function simply samples the track as one interval rather than creating redundant windows.

---

# Native grey-band acceptance

For a renderer-compatible FLAC whose mean is between the obvious-safe and obvious-hot boundaries, the current policy tests sampled metrics against:

```text
mean                 <= 1,475,000 bit/s
sampled P99          <= 1,850,000 bit/s
max 3-second average <= 1,700,000 bit/s
fraction > 1.5M      <= 25%
longest sampled run  <= 4 seconds
comparative debt     <= 512 KiB
```

These are **engineering policy values**, not claimed manufacturer hardware specifications.

They are intentionally multidimensional. A track does not pass merely because one number looks good.

---

# Adaptive candidate calibration

If transcoding is required, the wrapper still tries to preserve as much source precision as practical.

## 1. Choose target sample rate

The source first gets a renderer-compatible target rate according to the family policy described later.

## 2. Estimate a starting bit depth

The heuristic starts with the source mean rate and approximately adjusts for:

```text
target sample rate / source sample rate
2 channels / source channel count
```

Conceptually:

```text
normalised_load = source_mean
                  * target_rate/source_rate
                  * 2/source_channels
```

Then it estimates how many effective bits are likely to put the candidate around the desired average region.

This estimate is **not trusted as the final answer**. It only avoids blindly trying 24, 23, 22, 21, 20, 19... every time.

## 3. Locate a hard region

The worst sampled 3-second position is remembered.

The calibration start is:

```text
worst3 - 4 seconds
```

clamped to zero.

The wrapper then encodes a **12-second** segment around that difficult location.

This intentionally tests a hard region rather than a random easy section.

## 4. Encode a candidate

The candidate is resampled/downmixed if needed and quantised to a chosen effective precision, then encoded as FLAC level 8.

## 5. Measure the candidate itself

Because the candidate is only 12 seconds long, the script can inspect the whole candidate output cheaply.

It applies the same packet metrics to the encoded candidate.

## 6. Candidate acceptance policy

Version 4.1 currently requires:

```text
candidate mean       <= 1,475,000 bit/s
candidate P99        <= 1,650,000 bit/s
max 3-second average <= 1,525,000 bit/s
fraction > 1.5M      <= 35%
longest hot run      <= 2 seconds
comparative debt     <= 128 KiB
```

Why can a 12-second worst-case sample tolerate 35% of its seconds over the nominal 1.5-Mbit/s line?

Because it is **deliberately selected from the hottest area found**. Treating that 12 seconds as if it were a uniform random sample of the entire track made an earlier policy unnecessarily conservative.

The stronger signals are whether those excursions form sustained runs, whether the 3-second average remains controlled, and whether the debt model stays small.

## 7. Search direction

If the initial estimate passes, the script tries moving upward one effective bit at a time to preserve more precision until a candidate fails or the attempt limit is reached.

If the initial estimate fails, it moves downward until the first passing candidate is found.

There is a hard maximum of five candidate attempts.

That prevents an edge case from creating a long startup delay.

## 8. Fallback

If no 17-24-bit effective candidate passes within the attempt budget, the script uses the known-compatible 16-bit family-rate path and caches that decision.

---

# Effective-bit quantisation

This is the unusual part of the design.

The stream can remain a **nominal 24-bit FLAC**, while the sample values are restricted to a lower effective precision.

For example, the 20-effective-bit path uses:

```text
scale = 524288 = 2^19
```

and FFmpeg's `aeval` filter performs conceptually:

```text
quantised_sample = round(sample * scale) / scale
```

for every channel.

The script's scale table is:

```text
23 bits -> 4194304
22 bits -> 2097152
21 bits -> 1048576
20 bits ->  524288
19 bits ->  262144
18 bits ->  131072
17 bits ->   65536
```

Why does this help?

Lossless FLAC bitrate is driven partly by how predictable/compressible the PCM is. Removing unused low-order precision reduces entropy and gives FLAC more opportunity to compress the signal, without forcing every difficult source all the way down to 16-bit.

The FFmpeg filter used for an adaptive candidate is structurally:

```text
aresample=TARGET_RATE:ochl=stereo,
aeval=round(val(ch)*SCALE)/SCALE:c=same
```

and the result is encoded with:

```text
-sample_fmt s32
-c:a flac
-compression_level 8
```

## Why not just set `output_sample_bits`?

Testing showed that merely changing codec metadata/options did not perform the required real quantisation of the PCM values. The effective precision had to be changed in the sample data itself.

## Why not `acrusher`?

A tested `acrusher` approach increased entropy/encoded bitrate in this workload and did not produce the desired compression behaviour.

## Dither policy

The adaptive 17-23-effective-bit paths currently use deterministic rounding and **do not add dither**.

That is deliberate for this compatibility goal: added dither raises low-level entropy and therefore tends to increase a lossless codec's bitrate, working against the transport constraint being managed.

The true 16-bit fallback does use FFmpeg's high-pass triangular dither:

```text
dither_method=triangular_hp
```

Changing the dither strategy is possible, but it changes FLAC entropy and therefore requires re-validating the bitrate thresholds.

---

# Sample-rate policy

The renderer is treated as supporting a maximum native sample rate of 48 kHz for this compatibility policy.

For sources already at or below 48 kHz, the actual source rate is retained whenever practical.

For higher-rate PCM, the wrapper preserves the source's rate family:

```text
44.1 / 88.2 / 176.4 / 352.8 / 705.6 kHz family -> 44.1 kHz
48 / 96 / 192 / 384 / 768 kHz family            -> 48 kHz
```

For unusual rates above 48 kHz, divisibility by 44.1 kHz is used as a family hint; otherwise 48 kHz is selected.

Why preserve families?

It avoids converting a 44.1-family source to 48 kHz merely because 48 kHz is numerically higher. The extra 1.95 kHz of Nyquist bandwidth is rarely a useful trade for an unnecessary asynchronous family conversion in this renderer-compatibility use case.

---

# DSF/SACD policy

DSF follows a fixed path instead of the adaptive FLAC analysis:

```text
DSF / DSD
  -> first audio stream
  -> stereo
  -> 44.1 kHz
  -> 20 effective bits
  -> nominal 24-bit FLAC
  -> FLAC compression level 8
```

No calibration cache is needed because there is no per-file decision to rediscover.

## Why 44.1 kHz?

DSD64 is based on:

```text
2.8224 MHz = 44.1 kHz x 64
```

So 44.1 kHz is the natural PCM family target.

## Why stereo?

The Roberts is a stereo renderer for this purpose, and multichannel DSF must therefore be downmixed before delivery.

Direct FFmpeg testing established that the audio itself could be downmixed correctly. Gerbera's external wrapper path gave us control over the complete output stream rather than depending on stale source channel metadata from another transcoding layer.

## Why 20 effective bits?

Measured 24-bit/44.1-kHz FLAC from a difficult multichannel DSF source was too close to or above the practical sustained transport region. Genuine 20-effective-bit quantisation brought the FLAC stream into a much safer range while retaining substantially more precision than a blanket 16-bit conversion.

Twenty bits are **not** treated as a universal magic number. The adaptive FLAC path can choose 23, 22, 21, 20, 19, etc. based on the measured candidate.

---

# Why FLAC level 8

FLAC compression levels do not change decoded audio quality. They change encoder effort and output size.

In development, representative live encodes showed level 8 offering almost the compression of a slower maximum-effort setting while taking essentially the same time as a more moderate setting on the tested NAS.

So the live policy uses:

```text
FLAC_LEVEL=8
```

The exact CPU performance will vary by NAS model.

---

# Cache design and invalidation

First-play intelligence is only useful if it does not repeat forever.

The cache directory is:

```text
/var/run/gerbera/roberts-cache-v4
```

which sits inside the persistent host bind mount.

## Cache key

The source path is hashed. The clear-text path is not written into the cache filename.

## File signature

A separate signature is derived from `stat` information including:

- size;
- inode;
- numeric modification time;
- numeric change time;
- high-resolution timestamp forms where supported.

The signature is also hashed.

If the source changes, the cached decision no longer matches.

## Policy ID

The script contains a policy version such as:

```text
roberts-v4.1-20260817b
```

Changing thresholds/logic should also change this policy ID.

That invalidates old decisions without manually deleting the cache.

## Atomic writes

A new cache entry is first written to a temporary file and then moved into place.

Readers therefore do not see a half-written cache decision.

## Permissions

The cache directory is expected to be mode `0700` and cache files mode `0600` under the Gerbera user.

If the wrapper cannot establish that the directory is actually writable, caching is disabled for that request instead of pretending to work.

---

# Why the script uses `exec`

When the final streaming action has been chosen, functions use `exec ffmpeg ...` or `exec cat ...` rather than launching the final process as a child and leaving an extra shell sitting above it.

This matters because Gerbera needs to be able to terminate the external transcoder cleanly when playback is stopped.

Gerbera's own transcoding documentation explicitly warns wrapper authors to use `exec` for this reason.

---

# Worked example: difficult 24/48 FLAC

One development stress file had approximately:

```text
whole-track mean      1572.9 kbit/s
P95                   1777.1 kbit/s
P99                   1869.8 kbit/s
max 1-second          1962.5 kbit/s
max 3-second          1870.7 kbit/s
seconds over 1.5M     77.21%
longest hot run       525 seconds
```

That is not a “one freak packet” problem. It is sustained.

A long difficult region measured approximately:

```text
22 effective bits / 48 kHz -> still too hot
21 effective bits / 48 kHz -> marginal
20 effective bits / 48 kHz -> strong balance
19 effective bits / 48 kHz -> safer, but unnecessarily conservative
```

The live 12-second hotspot calibration initially made 20-bit look harsher than the longer validation region. Version 4.1 therefore changed the candidate criteria so that a short, deliberately selected worst-case sample is not judged as though it were a random whole-track sample.

In live testing the resulting 20-effective-bit/48-kHz FLAC could occasionally move slightly above the nominal 1.5-Mbit/s engineering line without playback interruption. This reinforced the central design conclusion: **short excursions are much less important than sustained pressure**.

---

# Worked example: DSF

A multichannel DSD64-family source showed why simply converting to nominal 24-bit PCM/FLAC was not always enough.

Development measurements were approximately:

```text
24-bit / 44.1 kHz stereo FLAC
  mean around 1.52 Mbit/s
  peaks around 1.72 Mbit/s

20-effective-bit / 44.1 kHz stereo FLAC, level 8
  mean around 1.29 Mbit/s
  peak around 1.49 Mbit/s
```

The latter played cleanly in the test environment and became the fixed DSF policy.

These numbers are examples from specific source material. They are not promises for every DSF file.

---

# Troubleshooting

## The Roberts cannot see Gerbera

Check host networking first.

In Container Station:

1. **Containers** -> click `gerbera`.
2. Stop it if Container Station requires a stop before network changes/recreation.
3. Use **Edit/Recreate** as supported by your Container Station version.
4. **Advanced Settings** -> **Networks**.
5. Confirm **Host** mode.

Also check:

```bash
docker logs --tail 100 gerbera
```

and confirm Gerbera reports a LAN bind address.

UPnP multicast can also be broken by router/AP “client isolation”, guest Wi-Fi separation, VLAN rules, multicast filtering, or IGMP settings. The server and renderer must be able to discover each other on the intended network.

## Browser cannot open port 49494

Check whether Gerbera started:

```bash
docker ps --filter name=gerbera
```

Check logs:

```bash
docker logs --tail 100 gerbera
```

With host networking, another NAS process occupying 49494 will conflict.

## Gerbera sees `/mnt/content` but cannot read files

The Docker mount exists, but the NAS ACL probably does not allow UID 1042 to traverse/read the tree.

Test a known file:

```bash
docker exec gerbera sh -c '
su-exec gerbera test -r "/mnt/content/path/to/a/known-file.flac" \
  && echo READ-OK \
  || echo READ-FAILED
'
```

If it fails, revisit the ACL section.

## Gerbera can write into the music directory

Stop immediately and fix the bind mount.

In Container Station, the host music path -> `/mnt/content` mapping must be **RO / Read Only**.

Filesystem ACLs alone are not the intended write barrier.

## `cache-dir NOT WRITABLE`

Inspect ownership:

```bash
docker exec gerbera \
  ls -ldn /var/run/gerbera/roberts-cache-v4
```

Expected owner/group are normally UID/GID 1042 in this setup.

Fix on the host:

```bash
sudo chown -Rv 1042:1042 \
  /share/Container/gerbera/roberts-cache-v4

sudo chmod -v 700 \
  /share/Container/gerbera/roberts-cache-v4
```

Then run the check correctly:

```bash
docker exec gerbera sh -c '
su-exec gerbera /var/run/gerbera/roberts-audio.sh check
'
```

## First play calibrates every time

That almost always means the cache decision is not being written or is being invalidated.

Look for:

```text
cache=miss
Permission denied
CACHE disabled
```

in logs.

If a first play succeeds and the source file is unchanged, a later request should show `CACHE HIT`.

## High-rate FLAC still pauses

Capture the wrapper log for that track:

```bash
docker logs -f --since 10s gerbera
```

The useful lines are:

```text
SOURCE
FAST DECISION / SAMPLED DECISION
SAMPLED SOURCE
CANDIDATE
CALIBRATE selected
TRANSCODE
CACHE HIT
```

Do not retune thresholds based on the radio's instantaneous displayed bitrate alone. A single display number does not describe sustained packet pressure.

## DSF sounds wrong or channels are strange

Confirm the request is going through the `dsf` wrapper mode and not another transcoding layer.

Expected log:

```text
TRANSCODE -> FLAC 20-bit-effective/44.1kHz/stereo ... fixed DSF/SACD policy
```

## Gerbera config refuses to load

Check syntax/startup logs:

```bash
docker logs --tail 150 gerbera
```

Remember:

- the live UDN must be a real unique value;
- the wrapper file must exist at `/var/run/gerbera/roberts-audio.sh`;
- the script must be executable;
- use the config schema/version appropriate to the pinned Gerbera release.

---

# Updating or recreating the container

One reason for separating state from the container is that the container itself becomes disposable.

Before any upgrade:

```bash
docker stop gerbera

sudo cp -av \
  /share/Container/gerbera/config.xml \
  /share/Container/gerbera/config.xml.pre-upgrade

sudo cp -av \
  /share/Container/gerbera/roberts-audio.sh \
  /share/Container/gerbera/roberts-audio.sh.pre-upgrade
```

The persistent directory and read-only music bind can then be attached to a recreated container.

After any Gerbera/image/FFmpeg upgrade, retest:

1. config loading;
2. UI access;
3. UPnP discovery;
4. read-only source mount;
5. `roberts-audio.sh check`;
6. native FLAC;
7. difficult FLAC first-play calibration;
8. cache hit on replay;
9. DSF.

Do not assume a new FFmpeg build will produce byte-for-byte identical FLAC bitrate characteristics. Lossless decoded audio may be equivalent while encoder heuristics and packetisation change enough to affect this transport-tuning problem.

---

# Optional future improvement: renderer-specific client flags

The config in this project places the Roberts-compatible transcoding resource first for the mapped media types.

Gerbera also supports **client flags**, allowing a transcoding mapping/profile to be selected only for identified clients.

That is worth considering if the same Gerbera instance serves both:

- the Roberts radio; and
- modern renderers that should always receive untouched originals.

A future deployment can identify the Roberts in Gerbera's **Clients** view and assign a dedicated transcoding flag, then add the matching `client-flags` attribute to the Roberts mappings/profiles.

Do this only after the basic system is stable; client identification strings and behaviour should be confirmed on the actual network rather than guessed.

---

# Public-GitHub safety checklist

Before pushing a repository public, check all files for:

```text
real NAS LAN IP addresses
public IP addresses
personal usernames
email addresses
real UDN/UUID values
real passwords
API tokens or keys
/share/... paths that reveal private folder names
music-library filenames if you consider them private
Gerbera SQLite databases
cache files
runtime logs
ACL backups
SSH keys
```

The public example should use placeholders such as:

```text
YOUR_NAS_LAN_ADDRESS
YOUR_QNAP_ADMIN_USER
/share/YourMusicShare/Music
uuid:my-long-uuid-change-me
gerbera-change-me
```

Recommended `.gitignore` entries:

```gitignore
# Gerbera runtime state
gerbera.db
gerbera.db.*
roberts-cache-v4/
*.cache
*.log

# Local/private copies
config.xml
music-acl-before.txt
*.generated-backup
*.pre-upgrade
```

Keep only the sanitized example in source control:

```text
config.example.xml
```

The wrapper itself contains engineering measurements and names of development test recordings. Those are not credentials and no audio data is included.

---

# Design principles recap

The final system follows a few rules that are more important than any single threshold.

## Preserve originals

The source music tree is read-only to the container.

## Pass native when safe

No transcoding should occur merely because transcoding is available.

## Bound first-play work

Do not scan a four-hour FLAC from beginning to end before pressing Play.

## Measure sustained stress

Mean, percentiles, rolling windows, hot-run length and debt tell a more useful story than a single maximum packet.

## Calibrate only the files that need it

A difficult file gets a short local encode, not an exhaustive whole-track search.

## Preserve the source rate family

44.1-family material stays in the 44.1 family; 48-family material stays in the 48 family when downsampling is required.

## Use the highest practical effective precision

20-bit happened to be the best answer for important development examples, but the algorithm is allowed to choose another depth.

## Cache the answer safely

An unchanged file should not pay the analysis cost twice.

## Fail conservative

If metadata/probing/calibration cannot establish a safe higher-precision stream within the bounded work budget, use a known-compatible fallback rather than stall indefinitely.

---

# Reference command sheet

## Stop / start

```bash
docker stop gerbera
docker start gerbera
```

## Recent logs

```bash
docker logs --tail 100 gerbera
```

## Live logs

```bash
docker logs -f --since 10s gerbera
```

## Correct wrapper check

```bash
docker exec gerbera sh -c '
su-exec gerbera /var/run/gerbera/roberts-audio.sh check
'
```

## Verify music mount is read-only

```bash
docker exec gerbera sh -c '
su-exec gerbera touch /mnt/content/GERBERA-WRITE-TEST
'
```

Expected: failure with `Read-only file system`.

## Inspect cache permissions

```bash
docker exec gerbera \
  ls -ldn /var/run/gerbera/roberts-cache-v4
```

## Verify live UDN

```bash
grep -n '<udn>' \
  /share/Container/gerbera/config.xml
```

Do not paste the resulting live UUID into a public issue unless you are comfortable exposing a persistent UPnP device identifier.

---

# References

The design and setup in this README were checked against the following upstream documentation.

## QNAP

- Container Station 3 tutorial:  
  <https://www.qnap.com/en-us/how-to/tutorial/article/how-to-use-container-station-3>
- Container Station Quick Start Guide:  
  <https://www.qnap.com/en-us/how-to/tutorial/article/container-station-quick-start-guide>
- QTS SSH configuration:  
  <https://docs.qnap.com/operating-system/qts/5.2.x/en-us/enabling-ssh-on-the-nas-61C229CD.html>

## Gerbera

- Gerbera documentation:  
  <https://docs.gerbera.io/en/stable/>
- Gerbera Docker README:  
  <https://github.com/gerbera/gerbera/blob/master/README.Docker.md>
- Gerbera Docker image:  
  <https://hub.docker.com/r/gerbera/gerbera>
- Transcoding overview:  
  <https://docs.gerbera.io/en/stable/transcoding.html>
- Transcoding configuration reference:  
  <https://docs.gerbera.io/en/stable/config-transcode.html>
- Import/autoscan configuration:  
  <https://docs.gerbera.io/en/stable/config-import.html>
- Gerbera web UI:  
  <https://docs.gerbera.io/en/stable/ui.html>

## Roberts

- Roberts Revival iStream 3 quick-start/support page:  
  <https://www.robertsradio.com/en-gb/support/quickstart-guides/revival-iStream-3-quickstart-guide>

---

# Final notes

This project is deliberately conservative about **data safety** and deliberately aggressive about **avoiding unnecessary transcoding**.

The NAS keeps the master files. Docker is denied write access to them. Gerbera provides UPnP discovery and streaming. FFprobe and AWK make a bounded decision. FFmpeg creates a live compatibility stream only when needed. The cache turns an expensive first decision into a cheap repeat decision.

That division of responsibilities is the real design win: the renderer gets something it can consume, the originals remain pristine, and the NAS does not waste time converting media that was already safe.

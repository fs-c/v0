- WM: [i3](https://i3wm.org/) This thing should run anywhere.
- Browser: [Iridium](https://iridiumbrowser.de/) if it is supported, or [ungoogled-chromium](https://github.com/Eloston/ungoogled-chromium). Stable fallback could be [Dillo](https://www.dillo.org/).
- Editor: vi/vim does the job, [VSCode](https://code.visualstudio.com/) for comfyness

To control backlight on Linux distributions: [light](https://github.com/haikarainen/light).

ShareX for Linux and FreeBSD: [sharenix](https://github.com/Francesco149/sharenix)

---

__Base system__

Debian netinst, disable root account in the installer (might have to choose expert mode). Use https://deb.debian.org/ as package mirror.

__Initial setup__

In `/etc/apt/sources.list`, use the `unstable` distribution.

```bash
$ sudo apt-get install aptitude
$ sudo aptitude update
$ sudo aptitude dist-upgrade
```

__Desktop setup__

To get the barebones system up and running.

```bash
$ sudo aptitude install xinit lightdm i3
```

Comfortable terminal, background management, miscellaneous applications.

```bash
$ sudo aptitude install tilix feh vim htop
```

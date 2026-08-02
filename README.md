
# .emacs.d

## Git 配置

`git` 本身配置:

```shell
git config --global core.preloadindex true
git config --global core.fscache true
git config --global gc.auto 256
```

`Windows` 上的 `Magit` 配置，使用 `custom.el`:

```elisp
(custom-set-variables
 '(magit-git-executable "C:/Program Files/Git/bin/git.exe"))
```

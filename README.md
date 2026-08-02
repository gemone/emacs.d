
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

## 代理配置

`custom.el` 里配置 HTTP/HTTPS 代理（`url-proxy-services`，elpaca 等网络包都走它）：

```elisp
(custom-set-variables
 '(url-proxy-services
   '(("http"     . "127.0.0.1:7890")
     ("https"    . "127.0.0.1:7890")
     ("no_proxy" . "^\\(localhost\\|192\\.168\\..*\\|10\\..*\\)"))))
```

- `"http"` / `"https"`: 代理地址，格式 `host:port`（按你本地代理改）
- `"no_proxy"`: 不走代理的地址（正则）
- 留空 `("http" . "")` 表示该协议不走代理；整个变量设为 `nil` 关闭代理

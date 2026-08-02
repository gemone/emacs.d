
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

## LLM 编程 agent

Emacs 内两个独立 agent 前端，按任务选其一：

- **codex** — [`emacs-codex-ide`](https://github.com/dgillis/emacs-codex-ide)，原生 Codex 客户端（`codex app-server`）。启动 `M-x codex-ide-menu`
- **pi** — [`pi-coding-agent`](https://github.com/dnouri/pi-coding-agent)，Pi CLI 的 Emacs 前端。启动 `M-x pi-coding-agent`

使用前需登录各自 CLI（凭证独立）：

- **codex**：终端 `codex` 完成登录，见 [codex 文档](https://code.claude.com/docs/en/overview)
- **pi**：终端 `pi` 完成 provider/API key 配置，凭证存 `~/.pi/agent/`，见 [pi 文档](https://github.com/dnouri/pi-coding-agent#install-pi-and-authenticate-)

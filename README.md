
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

## Eglot / LSP 支持

`init.el` 使用 Emacs 内置的 **Eglot**（29+ 自带）做 LSP 客户端，自动为 `prog-mode`
下除 `emacs-lisp-mode` 外的语言启动。注意区分两类东西：

- **tree-sitter 语法库**（treesit-auto 安装，如 `libtree-sitter-python.so`）：只负责解析/高亮/缩进
- **LSP 服务器**（如 `ty`、`ruff`、`jdtls`）：负责补全、诊断、跳转、格式化，必须单独安装

### 已配置语言

#### Python：`rass`（`ty` + `ruff`）

Eglot 每个 buffer 只能连接一个 LSP 服务器，所以用
[rassumfrassum](https://github.com/joaotavora/rassumfrassum)（`rass`）把类型检查
`ty server` 和 lint/格式化 `ruff server` 合并成一条 stdio 连接
（等价命令：`rass -- ty server -- ruff server`）。

```shell
uv tool install rassumfrassum ty ruff
```

`ty`/`ruff` 的规则写在项目根目录 `pyproject.toml` 的 `[tool.ty]` / `[tool.ruff]`
段（没有则用默认配置）。

#### Java：`jdtls`

`init.el` 已把 `java-mode` / `java-ts-mode` 指向 Eclipse JDT Language Server，
workspace 元数据放在 `~/.cache/emacs/jdtls-workspace/`，不污染项目目录。

```shell
# 任选一种安装方式（需要 Java 17+，本机当前未装 Java）
sdk install jdtls          # SDKMAN
# 或下载: https://download.eclipse.org/jdtls/snapshots/
# 或 Debian/Ubuntu: sudo apt install eclipse-jdt-ls
```

#### TypeScript / Angular / web-mode

- `.ts` / `.tsx`：Angular 项目（存在 `angular.json` 或 `project.json`）用
  `ngserver`（npm 包 `@angular/language-server` 提供的可执行文件），否则用
  `typescript-language-server`
- HTML 模板（`web-mode` / `html-mode` / `html-ts-mode`）：Angular 项目里用 `rass`
  合并 **ngserver + vscode-html-language-server + vscode-css-language-server**
  三个服务器；普通项目回退到默认 HTML 服务器

```shell
npm install -g @angular/language-server @angular/language-service \
  typescript typescript-language-server vscode-langservers-extracted
```

#### Emacs Lisp：无 LSP，用内置 `flymake`

`emacs-lisp-mode` 打开即启用 `flymake-mode`。配置文件场景下只保留
`elisp-flymake-checkdoc`（文档/风格）后端；`elisp-flymake-byte-compile`
（编译错误）会被移除，因为它的子进程 `load-path` 只有 `./`，看不到
Elpaca 安装的包，会对配置文件产生大量"函数未定义"噪音。

### 使用与快捷键

- 打开源文件自动连接（`M-x eglot-ensure` 挂在 `prog-mode-hook` 上）
- `C-c c a` 代码操作、`C-c c o` 整理 import、`C-c c r` 重命名、`C-c c f` 格式化
- 保存时若被 Eglot 管理则自动格式化（`before-save-hook`）
- 内联提示（inlay hints）默认开启，`M-x eglot-inlay-hints-mode` 可切换

### 验证与排错

- 模式栏出现 `[eglot:...]` 表示已连接
- `M-x eglot-events-buffer` 查看 LSP 通信日志
- `M-x eglot-show-workspace-configuration` 查看发给服务器的配置
- `M-x eglot-shutdown` / `eglot-reconnect` 重启连接

`init.el` 已把 `~/.local/bin`（`uv`/`npm` 全局工具目录）同时加入 `exec-path`
和子进程的 `PATH` 环境变量（`rass` 需要靠它再拉起 `ty`/`ruff`）。若在终端里
也想直接使用这些命令，可执行 `uv tool update-shell` 或手动加 PATH。

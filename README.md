
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

使用 [`agent-shell`](https://github.com/xenodium/agent-shell) 作为 Emacs 内统一的
agent.

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

#### Java：`eglot-java` + `dape` + `java-server`

完整的 Java 开发栈（参考 [Emacs China 论坛](https://emacs-china.org/t/emacs-eglot-eglot-java-dape-java/30086)
与 [LuciusChen/java-server](https://github.com/LuciusChen/java-server)）：

- **`eglot-java`**：jdtls 的 eglot 扩展。首次打开 `.java` 时自动下载
  `eclipse.jdt.ls`（`M-x eglot-java-upgrade-lsp-server` 可手动升级），提供
  新建工程/类、Maven/Gradle 构建任务、JUnit 运行等命令
- **`dape`**：DAP 调试客户端。内置 `jdtls` 配置可直接 launch 主类；另配
  `jdtls-jpda` 配置 attach 到 JUnit/外置 Tomcat 的 JPDA 端口
  （链路：dape → java-debug 适配器（由 JDTLS 拉起）→ 目标 JVM）
- **`java-server`**：多 JDK 切换、外置 Tomcat 部署/停止、Spring Boot 启停、
  热替换（HCR）

依赖安装（本机当前未装 Java 工具链，需先装 JDK）：

```shell
# JDK 17+（jdtls 自身要求）
sdk install java 17.x          # 或用系统包管理器
# jdtls —— eglot-java 首次打开 .java 时会自动下载，也可手动：
sdk install jdtls
# java-debug 插件 jar（dape 调试必需）
git clone https://github.com/microsoft/java-debug /tmp/java-debug
cd /tmp/java-debug && mvn -DskipTests package
```

`java-debug` 的 jar 产物在 `extension/server/` 下，放到 `my/java-debug-plugin-jar`
查找的位置即可（如 `~/.cache/emacs/java-debug/`、`~/java-debug/`、`/tmp/java-debug/`），
`init.el` 会通过 `:bundles` 初始化选项让 jdtls 加载它。外置 Tomcat 仅在需要
部署 WAR 时安装（macOS：`brew install tomcat@9`）。

jdtls 的 workspace 元数据放在 `~/.cache/emacs/jdtls-workspace/`，不污染项目目录。

快捷键：

| 键位 | 功能 |
|---|---|
| `C-c j t` / `C-c j m` | 运行 JUnit 测试 / 主类（`C-u` 前缀进 debug） |
| `C-c j N` / `C-c j n` | 新建项目 / 新建 Java 文件 |
| `C-c j T` / `C-c j R` | 构建任务 / 刷新项目 |
| `C-c j u` / `C-c j U` | 升级 jdtls / JUnit console jar |
| `C-c J j` / `C-c J a` | 手动 / 按 `pom.xml` 自动选择 JDK |
| `C-c J t` / `C-c J s` | 部署 Tomcat / 启动 Spring Boot（`C-u` 开启 JPDA 并自动 attach dape） |
| `C-c J h` | 热替换（HCR） |
| `C-c d` | dape 调试键前缀；`M-x dape` 选择配置（`jdtls` launch、`jdtls-jpda` attach 等） |

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

#### Markdown：代码块编辑

- 围栏代码块用语言自身的 major mode 高亮（`markdown-fontify-code-blocks-natively`，
  tree-sitter 优先）
- `C-c '`（`markdown-edit-code-block`，依赖 `edit-indirect`）在间接 buffer 中
  编辑代码块，`C-c C-c` 写回
- 自动推断不出的语言别名通过 `markdown-code-lang-modes` 补充
  （如 `ts` → `typescript-ts-mode`、`js` → `js-ts-mode`）

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

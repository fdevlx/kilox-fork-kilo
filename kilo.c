/**************\
  * includes *
\**************/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*************\
  * defines *
\*************/

#define KILO_VERSION "0.0.1"
#define KILO_TAB_STOP 4
#define KILO_QUIT_TIMES 3

#define CTRL_KEY(k) ((k) & 0x1f)

#define LINE_NUMBER_WIDTH 5

enum editorKey {
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};

enum editorHighlight {
    HL_NORMAL = 0,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_STRING,
    HL_NUMBER,
    HL_MATCH,
    HL_MATCH_BRACKET
};

enum editorMode {
    MODE_INSERT = 0,
    MODE_NORMAL
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/**********\
  * data *
\**********/

struct editorSyntax {
    char *filetype;
    char **filematch;
    char **keywords;
    char *singleline_comment_start;
    char *multiline_comment_start;
    char *multiline_comment_end;
    int flags;
};

typedef struct erow {
    int idx;
    int size;
    int rsize;
    char *chars;
    char *render;
    unsigned char *hl;
    int hl_open_comment;
} erow;

struct editorConfig {
    char *filename;
    char statusmsg[80];
    erow *row;
    int coloff;
    int cx, cy;
    int dirty;
    int mode;
    int numrows;
    int rowoff;
    int rx;
    int screencols;
    int screenrows;
    time_t statusmsg_time;
    struct editorSyntax *syntax;
    struct termios orig_termios;
};

struct editorConfig E;

/****************\
  * filetypes *
\****************/

char *C_HL_extensions[] = {".c", ".h", ".cpp", NULL};
char *C_HL_keywords[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "struct", "union", "typedef", "static", "enum", "class", "case",

    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
    "void|", NULL
};
char *PHP_extensions[] = { ".php", ".phtml", ".php3", ".php4", ".php5", ".phps", NULL };
char *PHP_keywords[] = {
    "abstract", "and", "array", "as", "break", "callable", "case", "catch", "class",
    "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else",
    "elseif", "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch",
    "endwhile", "eval", "exit", "extends", "final", "finally", "for", "foreach",
    "function", "global", "goto", "if", "implements", "include", "include_once",
    "instanceof", "insteadof", "interface", "isset", "list", "namespace", "new",
    "or", "print", "private", "protected", "public", "require", "require_once",
    "return", "static", "switch", "throw", "trait", "try", "unset", "use", "var",
    "while", "xor", "yield", "int|", "float|", "bool|", "string|", "object|", "void|", NULL
};
char *python_extensions[] = { ".py", ".pyw", ".pyd", ".pyi", NULL };
char *python_keywords[] = {
    "and", "as", "assert", "break", "class", "continue", "def", "del", "elif", "else",
    "except", "False", "finally", "for", "from", "global", "if", "import", "in", "is",
    "lambda", "None", "nonlocal", "not", "or", "pass", "raise", "return", "True",
    "try", "while", "with", "yield",
    "int|", "float|", "list|", "dict|", "set|", "str|", "print|", "range|", NULL
};
char *perl_extensions[] = { ".pl", ".pm", ".t", ".pod", NULL };
char *perl_keywords[] = {
    "if", "else", "elsif", "unless", "switch", "case", "default", "while", "until",
    "for", "foreach", "next", "last", "redo", "goto", "sub", "my", "our", "local",
    "die", "warn", "eval", "require", "use", "no", "return", "exit", "print|",
    "say|", "shift|", "push|", "pop|", "split|", "join|", NULL
};
// JavaScript
char *js_extensions[] = { ".js", ".mjs", ".cjs", ".json", NULL };
char *js_keywords[] = {
    "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete",
    "do", "else", "export", "extends", "finally", "for", "function", "if", "import", "in",
    "instanceof", "new", "return", "super", "switch", "this", "throw", "try", "typeof",
    "var", "void", "while", "with", "yield", "let|", "static|", "async|", "await|",
    "true|", "false|", "null|", "undefined|", NULL
};

// YAML
char *yaml_extensions[] = { ".yaml", ".yml", NULL };
char *yaml_keywords[] = { "true|", "false|", "null|", "yes|", "no|", NULL };

// Twig (Engine de Symfony)
char *twig_extensions[] = { ".twig", ".html.twig", NULL };
char *twig_keywords[] = {
    "apply", "autoescape", "block", "deprecated", "do", "embed", "extends", "flush", "for",
    "from", "if", "import", "include", "macro", "sandbox", "set", "use", "verbatim",
    "with", "endfor|", "endif|", "endblock|", NULL
};

char *html_extensions[] = { ".html", ".htm", ".php", ".twig", NULL };
char *html_keywords[] = {
    "class", "id", "href", "src", "style", "value", "type", "name", "rel", "alt",
    "div|", "span|", "a|", "img|", "script|", "link|", "html|", "body|", NULL
};

// .env
char *env_extensions[] = { ".env", ".env.local", ".env.dev", ".env.prod", NULL };
char *env_keywords[] = { "PORT|", "DEBUG|", "DATABASE_URL|", NULL };

struct editorSyntax HLDB[] = {
    {
        "c",
        C_HL_extensions,
        C_HL_keywords,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
    {
        "php",
        PHP_extensions,
        PHP_keywords,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
    {
        "python",
        python_extensions,
        python_keywords,
        "#", NULL, NULL,
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
    {
        "perl",
        perl_extensions,
        perl_keywords,
        "#", NULL, NULL,
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
    {
        "javascript",
        js_extensions,
        js_keywords,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
    {
        "yaml",
        yaml_extensions,
        yaml_keywords,
        "#", NULL, NULL,
        HL_HIGHLIGHT_STRINGS // YAML suele omitir el resaltado de números puros para evitar errores
    },
    {
        "twig",
        twig_extensions,
        twig_keywords,
        "{#", "{#", "#}",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
    {
        "env",
        env_extensions,
        env_keywords,
        "#", NULL, NULL,
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
    {
        "html",
        html_extensions,
        html_keywords,
        NULL, "<!--", "-->", // Comentarios multilínea HTML
        HL_HIGHLIGHT_STRINGS | HL_HIGHLIGHT_NUMBERS
    },
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/****************\
  * prototypes *
\****************/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));
void editorMoveCursor(int key);
int findMatchingBracket(int row_idx, int col_idx);

/**************\
  * terminal *
\**************/

void die(const char *s) {
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);

    perror(s);
    exit(1);
}

void disableRawMode() {
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1) {
        die("tcsetattr");
    }
}

void enableRawMode() {
    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
    atexit(disableRawMode);

    struct termios raw = E.orig_termios;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
        die("tcsetattr");
    }
}

int editorReadKey() {
    int nread;
    char c;
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN) die("read");
    }

    if (c == '\x1b') {
        char seq[3];

        if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
        if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

        if (seq[0] == '[') {
            if (seq[1] >= '0' && seq[1] <= '9') {
                if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
                if (seq[2] == '~') {
                    switch (seq[1]) {
                        case '1': return HOME_KEY;
                        case '3': return DEL_KEY;
                        case '4': return END_KEY;
                        case '5': return PAGE_UP;
                        case '6': return PAGE_DOWN;
                        case '7': return HOME_KEY;
                        case '8': return END_KEY;
                    }
                }
            } else {
                switch (seq[1]) {
                    case 'A': return ARROW_UP;
                    case 'B': return ARROW_DOWN;
                    case 'C': return ARROW_RIGHT;
                    case 'D': return ARROW_LEFT;
                    case 'H': return HOME_KEY;
                    case 'F': return END_KEY;
                }
            }
        } else if (seq[0] == 'O') {
            switch (seq[1]) {
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
            }
        }

        return '\x1b';
    } else {
    return c;
    }
}

int getCursorPosition(int *rows, int *cols) {
      char buf[32];
      unsigned int i = 0;

      if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

      while (i < sizeof(buf) - 1) {
        if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
      }
      buf[i] = '\0';

      if (buf[0] != '\x1b' || buf[1] != '[') return -1;
      if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

      return 0;
}

int getWindowSize(int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
        // ^ backup in case ioctl fails
        return getCursorPosition(rows, cols);
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }
}

/*************************\
  * syntax highlighting *
\*************************/

int is_separator(int c) {
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize);

    if (E.syntax == NULL) return;

    char **keywords = E.syntax->keywords;

    char *scs = E.syntax->singleline_comment_start;
    char *mcs = E.syntax->multiline_comment_start;
    char *mce = E.syntax->multiline_comment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs) : 0;
    int mce_len = mce ? strlen(mce) : 0;

    int prev_sep = 1;
    int in_string = 0;
    int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

    int i = 0;
    int in_tag = 0;

    while (i < row->rsize) {
        char c = row->render[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        if (!in_string && !in_tag && !in_comment) {
            if (!strncmp(&row->render[i], "<!--", 4)) {
                memset(&row->hl[i], HL_MLCOMMENT, 4);
                i += 4;
                in_comment = 1; // Reutilizamos in_comment para persistencia
                continue;
            }
        }

        if (!in_string && !in_comment) {
            if (c == '<') { in_tag = 1; row->hl[i] = HL_KEYWORD2; i++; continue; }
            if (c == '>') { in_tag = 0; row->hl[i] = HL_KEYWORD2; i++; continue; }
        }

        // 3. Strings (siempre alta prioridad para que class="valor" se vea bien)
        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;
                if (c == '\\' && i + 1 < row->rsize) { row->hl[i + 1] = HL_STRING; i += 2; continue; }
                if (c == in_string) in_string = 0;
                i++; prev_sep = 1; continue;
            } else if (c == '"' || c == '\'') {
                in_string = c; row->hl[i] = HL_STRING; i++; continue;
            }
        }

        // 4. Atributos dentro de la etiqueta (Keywords)
        // Movemos la lógica de keywords aquí arriba para que funcione DENTRO de in_tag
        if (prev_sep && keywords) {
            int j;
            for (j = 0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == '|';
                if (kw2) klen--;

                if (!strncmp(&row->render[i], keywords[j], klen) &&
                    is_separator(row->render[i + klen])) {
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                i += klen;
                break;
                    }
            }
            if (keywords[j] != NULL) { prev_sep = 0; continue; }
        }

        // 5. Si nada de lo anterior atrapó el carácter pero estamos en una etiqueta
        if (in_tag && !in_string && !in_comment) {
            row->hl[i] = HL_KEYWORD2; // Color de la etiqueta (Cian)
            i++;
            continue;
        }

        if (scs_len && !in_string && !in_comment) {
            if (!strncmp(&row->render[i], scs, scs_len)) {
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
                break;
            }
        }

        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_MLCOMMENT;
                if (!strncmp(&row->render[i], mce, mce_len)) {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;
                if (c == '\\' && i + 1 < row->rsize) {
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }
                if (c == in_string) in_string = 0;
                i++;
                prev_sep = 1;
                continue;
            } else {
                if (c == '"' || c == '\'') {
                    in_string = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    continue;
                }
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
                (c == '.' && prev_hl == HL_NUMBER)) {
                row->hl[i] = HL_NUMBER;
                i++;
                prev_sep = 0;
                continue;
            }
        }

        if (prev_sep) {
            int j;
            for (j = 0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == '|';
                if (kw2) klen--;

                if (!strncmp(&row->render[i], keywords[j], klen) &&
                        is_separator(row->render[i + klen])) {
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;
                    break;
                }
            }
            if (keywords[j] != NULL) {
                prev_sep = 0;
                continue;
            }
        }

        prev_sep = is_separator(c);
        i++;
    }
    int changed = (row->hl_open_comment != in_comment);
    row->hl_open_comment = in_comment;
    if (changed && row->idx + 1 < E.numrows) {
        editorUpdateSyntax(&E.row[row->idx + 1]);
    }
}

int editorSyntaxToColor(int hl) {
    switch (hl) {
        case HL_COMMENT: 
        case HL_MLCOMMENT: return 36;
        case HL_KEYWORD1: return 33;
        case HL_KEYWORD2: return 32;
        case HL_STRING: return 35;
        case HL_NUMBER: return 31;
        case HL_MATCH: return 34;
        default: return 37;
    }
}

void editorSelectSyntaxHighlight() {
    E.syntax = NULL;
    if (E.filename == NULL) return;

    char *ext = strrchr(E.filename, '.');

    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct editorSyntax *s = &HLDB[j];
        unsigned int i = 0;
        while (s->filematch[i]) {
            int is_ext = (s->filematch[i][0] == '.');
            if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
                    (!is_ext && strstr(E.filename, s->filematch[i]))) {
                E.syntax = s;

                int filerow;
                for (filerow = 0; filerow < E.numrows; filerow++) {
                    editorUpdateSyntax(&E.row[filerow]);
                }

                return;
            }
            i++;
        }
    }
}

/********************\
  * row operations *
\********************/

int editorRowCxToRx(erow *row, int cx) {
    int rx = 0;
    int j;
    for (j = 0; j < cx; j++) {
        if (row->chars[j] == '\t') {
            rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
        }
        rx++;
    }
    return rx;
}

int editorRowRxToCx(erow *row, int rx) {
    int cur_rx = 0;
    int cx;
    for (cx = 0; cx < row->size; cx++) {
        if (row->chars[cx] == '\t') {
            cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
        }
        cur_rx++;

        if (cur_rx > rx) return cx;
    }
    return cx;
}

void editorUpdateRow(erow *row) {
    int tabs = 0;
    int j;
    for (j = 0; j < row->size; j++)
        if (row->chars[j] == '\t') tabs++;

    free(row->render);
    row->render = malloc(row->size + tabs*(KILO_TAB_STOP - 1) + 1);

    int idx = 0;
    for (j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
            row->render[idx++] = ' ';
            while (idx % KILO_TAB_STOP != 0) row->render[idx++] = ' ';
        } else {
            row->render[idx++] = row->chars[j];
        }
    }
    row->render[idx] = '\0';
    row->rsize = idx;

    editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
    if (at < 0 || at > E.numrows) return;

    E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
    memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
    for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;

    E.row[at].idx = at;

    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';

    E.row[at].rsize = 0;
    E.row[at].render = NULL;
    E.row[at].hl = NULL;
    E.row[at].hl_open_comment = 0;
    editorUpdateRow(&E.row[at]);

    E.numrows++;
    E.dirty++;
}

void editorFreeRow(erow *row) {
    free(row->render);
    free(row->chars);
    free(row->hl);
}

void editorDelRow(int at) {
    if (at < 0 || at >= E.numrows) return;
    editorFreeRow(&E.row[at]);
    memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
    for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--;
    E.numrows--;
    E.dirty++;
}

void editorRowInsertChar(erow *row, int at, int c) {
    if (at < 0 || at > row->size) at = row->size;
    row->chars = realloc(row->chars, row->size + 2);
    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    editorUpdateRow(row);
    E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
    E.dirty++;
}

void editorRowDelChar(erow *row, int at) {
    if (at < 0 || at >= row->size) return;
    memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;
    editorUpdateRow(row);
    E.dirty++;
}

/***********************\
  * editor operations *
\***********************/

void editorInsertChar(int c) {
    if (E.cy == E.numrows) {
        editorInsertRow(E.numrows, "", 0);
    }
    editorRowInsertChar(&E.row[E.cy], E.cx, c);
    E.cx++;
}

void editorInsertNewline() {
    erow *row = &E.row[E.cy];
    int indentation = 0;

    // 1. Calculamos la indentación actual (antes de romper la línea)
    char *current_chars = malloc(row->size + 1);
    memcpy(current_chars, row->chars, row->size);
    while (indentation < row->size && (current_chars[indentation] == ' ' || current_chars[indentation] == '\t')) {
        indentation++;
    }

    // 2. Tu lógica original para romper la línea
    if (E.cx == 0) {
        editorInsertRow(E.cy, "", 0);
    } else {
        editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.row[E.cy];
        row->size = E.cx;
        row->chars[row->size] = '\0';
        editorUpdateRow(row);
    }

    E.cy++;
    E.cx = 0;

    // 3. Aplicamos la indentación heredada
    for (int i = 0; i < indentation; i++) {
        editorInsertChar(current_chars[i]);
    }
    free(current_chars);

    // 4. AQUÍ VA EL BLOQUE DE LA LLAVE '{' (PHP/JS Style)
    if (E.cy > 0) { // Verificamos que no sea la primera línea
        erow *prev_row = &E.row[E.cy - 1];
        if (prev_row->size > 0 && prev_row->chars[prev_row->size - 1] == '{') {
            editorInsertChar('\t'); // O 4 espacios
        }
    }
}

void editorDelChar() {
    if (E.cy == E.numrows) return;
    if (E.cx == 0 && E.cy == 0) return;

    erow *row = &E.row[E.cy];
    if (E.cx > 0) {
        editorRowDelChar(row, E.cx - 1);
        E.cx--;
    } else {
        E.cx = E.row[E.cy - 1].size;
        editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
        editorDelRow(E.cy);
        E.cy--;
    }
}

/**************\
  * file i/o *
\**************/

char *editorRowsToString(int *buflen) {
    int totlen = 0;
    int j;
    for (j = 0; j < E.numrows; j++) {
        totlen += E.row[j].size + 1;
    }
    *buflen = totlen;

    char *buf = malloc(totlen);
    char *p = buf;
    for (j = 0; j < E.numrows; j++) {
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        p++;
    }

    return buf;
}

void editorOpen(char *filename) {
    free(E.filename);
    E.filename = strdup(filename);

    editorSelectSyntaxHighlight();

    FILE *fp = fopen(filename, "r");
    if (!fp) die("fopen");

    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen;
    while ((linelen = getline(&line, &linecap, fp)) != -1) {
        while (linelen > 0 && (line[linelen - 1] == '\n' ||
                               line[linelen - 1] == '\r'))
            linelen--;
        editorInsertRow(E.numrows, line, linelen);
    }
    free(line);
    fclose(fp);
    E.dirty = 0;
}

void editorSave() {
    if (E.filename == NULL) {
        E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
        if (E.filename == NULL) {
            editorSetStatusMessage("Save aborted");
            return;
        }
        editorSelectSyntaxHighlight();
    }

    int len;
    char *buf = editorRowsToString(&len);

    // O_TRUNC vacía el archivo automáticamente al abrirlo
    int fd = open(E.filename, O_RDWR | O_CREAT | O_TRUNC, 0644);
    if (fd == -1) {
        editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
        free(buf);
        return;
    }

    if (write(fd, buf, len) == len) {
        close(fd);
        free(buf);
        E.dirty = 0;
        editorSetStatusMessage("%d bytes written to disk", len);
    } else {
        close(fd);
        free(buf);
        editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
    }
}

/**********\
  * find *
\**********/

void editorFindCallback(char *query, int key) {
    static int last_match = -1;
    static int direction = 1;

    static int saved_hl_line;
    static char *saved_hl = NULL;

    if (saved_hl) {
        memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
        free(saved_hl);
        saved_hl = NULL;
    }

    if (key == '\r' || key == '\x1b') {
        last_match = -1;
        direction = 1;
        return;
    } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
        direction = 1;
    } else if (key == ARROW_LEFT || key == ARROW_UP) {
        direction = -1;
    } else {
        last_match = -1;
        direction = 1;
    }

    if (last_match == -1) direction = 1;
    int current = last_match;
    int i;
    for (i = 0; i < E.numrows; i++) {
        current += direction;
        if (current == -1) current = E.numrows - 1;
        else if (current == E.numrows) current = 0;

        erow *row = &E.row[current];
        char *match = strstr(row->render, query);
        if (match) {
            last_match = current;
            E.cy = current;
            E.cx = editorRowRxToCx(row, match - row->render);
            //E.rowoff = E.numrows;

            saved_hl_line = current;
            saved_hl = malloc(row->rsize);
            memcpy(saved_hl, row->hl, row->rsize);
            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            break;
        }
    }
}

void editorFind() {
    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_coloff = E.coloff;
    int saved_rowoff = E.rowoff;

    char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)", 
                            editorFindCallback);

    if (query) {
        free(query);
    } else {
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.coloff = saved_coloff;
        E.rowoff = saved_rowoff;
    }
}

/*******************\
  * append buffer *
\*******************/

// buffer struct is to prevent
// many small write() calls
struct abuf {
    char *b;
    int len;
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf *ab, const char *s, int len) {
    char *new = realloc(ab->b, ab->len + len);

    if (new == NULL) return;
    memcpy(&new[ab->len], s, len);
    ab->b = new;
    ab->len += len;
}

void abFree(struct abuf *ab) {
    free(ab->b);
}

/************\
  * output *
\************/

void editorScroll() {
    E.rx = 0;
    if (E.cy < E.numrows) {
        E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
    }

    if (E.cy < E.rowoff) {
        E.rowoff = E.cy;
    }
    if (E.cy >= E.rowoff + E.screenrows) {
        E.rowoff = E.cy - E.screenrows + 1;
    }
    if (E.rx < E.coloff) {
        E.coloff = E.rx;
    }
    if (E.rx >= E.coloff + E.screencols) {
        E.coloff = E.rx - E.screencols + 1;
    }
}

void editorDrawRows(struct abuf *ab) {
    int y;
    // 1. Calculamos la posición del paréntesis compañero una sola vez por pantalla
    // para no ralentizar el bucle.
    int match_pos = findMatchingBracket(E.cy, E.cx);
    int match_row = (match_pos != -1) ? match_pos / 1000 : -1;
    int match_col = (match_pos != -1) ? match_pos % 1000 : -1;

    for (y = 0; y < E.screenrows; y++) {
        int filerow = y + E.rowoff;
        int is_current_line = (filerow == E.cy && filerow < E.numrows);

        if (filerow >= E.numrows) {
            // ... (Tu código de bienvenida se mantiene igual) ...
            if (E.numrows == 0 && y == E.screenrows / 3) {
                char welcome[80];
                int welcomelen = snprintf(welcome, sizeof(welcome), "Kilo editor -- version %s", KILO_VERSION);
                if (welcomelen > E.screencols) welcomelen = E.screencols;
                int padding = (E.screencols - LINE_NUMBER_WIDTH - welcomelen) / 2;
                if (padding) { abAppend(ab, "~", 1); padding--; }
                while (padding--) abAppend(ab, " ", 1);
                abAppend(ab, welcome, welcomelen);
            } else {
                abAppend(ab, "~", 1);
            }
        } else {
            if (is_current_line) {
                abAppend(ab, "\x1b[48;5;236m", 11); // Gris muy oscuro
            }
            // Dibujar números de línea
            char num[16];
            int numlen = snprintf(num, sizeof(num), "\x1b[90m %*d \x1b[39m", LINE_NUMBER_WIDTH - 2, filerow + 1);
            abAppend(ab, num, numlen);

            int visual_cols = E.screencols - LINE_NUMBER_WIDTH;
            int len = E.row[filerow].rsize - E.coloff;
            if (len < 0) len = 0;
            if (len > visual_cols) len = visual_cols;

            char *c = &E.row[filerow].render[E.coloff];
            unsigned char *hl = &E.row[filerow].hl[E.coloff];
            int current_color = -1;
            int j;
            for (j = 0; j < len; j++) {
                int char_col = j + E.coloff; // Columna real en el render

                // --- NUEVA LÓGICA DE RESALTADO DE PARÉNTESIS ---
                // Si es el que está bajo el cursor O su pareja
                if ((filerow == E.cy && char_col == E.rx) || (filerow == match_row && char_col == match_col)) {
                    // Usamos color de fondo (7m es invertido) o Magenta brillante de Breeze
                    abAppend(ab, "\x1b[1;35m", 7);
                    abAppend(ab, &c[j], 1);
                    abAppend(ab, "\x1b[m", 3); // Reset

                    if (is_current_line) abAppend(ab, "\x1b[48;5;236m", 11);
                    // Restaurar el color que venía aplicando la sintaxis
                    if (current_color != -1) {
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        abAppend(ab, buf, clen);
                    }
                    continue; // Pasamos al siguiente carácter
                }
                // --- FIN LÓGICA PARÉNTESIS ---

                if (iscntrl(c[j])) {
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?';
                    abAppend(ab, "\x1b[7m", 4);
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3);
                    if (current_color != -1) {
                        char buf[16];
                        int clen = (current_color == 32 || current_color == 33) ?
                        snprintf(buf, sizeof(buf), "\x1b[1;%dm", current_color) :
                        snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        abAppend(ab, buf, clen);
                    }
                } else if (hl[j] == HL_NORMAL) {
                    if (current_color != -1) {
                        abAppend(ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    abAppend(ab, &c[j], 1);
                } else {
                    int color = editorSyntaxToColor(hl[j]);
                    if (color != current_color) {
                        current_color = color;
                        char buf[16];
                        int clen = (current_color == 32 || current_color == 33) ?
                        snprintf(buf, sizeof(buf), "\x1b[1;%dm", current_color) :
                        snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        abAppend(ab, buf, clen);
                    }
                    abAppend(ab, &c[j], 1);
                }
            }
            abAppend(ab, "\x1b[39m", 5);
            if (is_current_line) {
                abAppend(ab, "\x1b[K", 3);    // Pinta el fondo hasta el final de la pantalla
                abAppend(ab, "\x1b[49m", 5);  // Reset color fondo
            }
        }
        abAppend(ab, "\x1b[K", 3);
        abAppend(ab, "\r\n", 2);
    }
}

void editorDrawStatusBar(struct abuf *ab) {
    abAppend(ab, "\x1b[K", 3);

    if (E.mode == 1) { // Asumiendo 1 para NORMAL
        abAppend(ab, "\x1b[1;37;44m", 10);
    } else { // INSERT
        abAppend(ab, "\x1b[1;37;42m", 10);
    }

    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), " %s | %.20s - %d lines %s",
            E.mode ? "NORMAL" : "INSERT", 
            E.filename ? E.filename : "[No Name]", 
            E.numrows, E.dirty ? "(modified)" : "");
    int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d ",
            E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);
    if (len > E.screencols) len = E.screencols;
    abAppend(ab, status, len);
    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            abAppend(ab, rstatus, rlen);
            break;
        } else {
            abAppend(ab, " ", 1);
            len++;
        }
    }
    abAppend(ab, "\x1b[m", 3);
    abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
    abAppend(ab, "\x1b[K", 3);
    int msglen = strlen(E.statusmsg);
    if (msglen > E.screencols) msglen = E.screencols;
    if (msglen && time(NULL) - E.statusmsg_time < 5){
        abAppend(ab, "\x1b[1m", 4); // Texto en negrita para resaltar
        abAppend(ab, E.statusmsg, msglen);
        abAppend(ab, "\x1b[m", 3);  // Resetear color
    }
}

void editorRefreshScreen() {
    editorScroll();

    struct abuf ab = ABUF_INIT;

    abAppend(&ab, "\x1b[?25l", 6);  // hides cursor
    abAppend(&ab, "\x1b[H", 3);     // moves cursor to first pos

    editorDrawRows(&ab);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);

    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1,
                                              (E.rx - E.coloff) + 1 + LINE_NUMBER_WIDTH);
    // ^ move cursor to y,x pos
    abAppend(&ab, buf, strlen(buf));

    abAppend(&ab, "\x1b[?25h", 6);  // unhides cursor

    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}

/***********\
  * input *
\***********/

int isStopChr(int c, char *s) {
    for (int i = 0; s[i] != '\0'; i += 1) {
        if (c == s[i]) {
            return 1;
        } else if (c == '\0') {
            return 1;
        }
    }
    return 0;
}

void editorSpecialMovement(int key) {
    char *stopChars = " '\"\n()[].#<>";

    switch (key) {
        case 'w':
            while (!isStopChr(E.row[E.cy].chars[E.cx], stopChars)) {
                editorMoveCursor(ARROW_RIGHT);
                while (isStopChr(E.row[E.cy].chars[E.cx + 1], stopChars)) {
                    editorMoveCursor(ARROW_RIGHT);
                }
            }
            editorMoveCursor(ARROW_RIGHT);
            break;
        case 'b':
            while (!isStopChr(E.row[E.cy].chars[E.cx], stopChars)) {
                editorMoveCursor(ARROW_LEFT);
                while (isStopChr(E.row[E.cy].chars[E.cx - 1], stopChars)) {
                    editorMoveCursor(ARROW_LEFT);
                }
            }
            editorMoveCursor(ARROW_LEFT);
            break;

        case '}':
            E.cx = 0;
            editorMoveCursor(ARROW_DOWN);
            while (E.row[E.cy].size != 0) {
                editorMoveCursor(ARROW_DOWN);
            }
            break;
        case '{':
            E.cx = 0;
            editorMoveCursor(ARROW_UP);
            while (E.row[E.cy].size != 0) {
                editorMoveCursor(ARROW_UP);
            }
            break;
    }
}

int editorNormalMovement(int key) {
    switch (key) {
        case 'h': return ARROW_LEFT; break;
        case 'j': return ARROW_DOWN; break;
        case 'k': return ARROW_UP; break;
        case 'l': return ARROW_RIGHT; break;
        case '\r': return ARROW_DOWN; break;
        case ' ': return ARROW_RIGHT; break;
        case BACKSPACE: return ARROW_LEFT; break;
        default: return key;
    }
}

void editorDoInsert(int key) {
    switch (key) {
        case 'i':
            E.mode = 0;
            break;
        case 'I': 
            E.cx = 0;
            E.mode = 0;
            break;
        case 'a': 
            editorMoveCursor(ARROW_RIGHT);
            E.mode = 0;
            break;
        case 'A': 
            if (E.cy < E.numrows) {
                E.cx = E.row[E.cy].size;
            }
            E.mode = 0;
            break;
        case 'o':
            if (E.cy < E.numrows) {
                E.cx = E.row[E.cy].size;
            }
            editorInsertNewline();
            E.mode = 0;
            break;
        case 'O':
            E.cx = 0;
            editorInsertNewline();
            editorMoveCursor(ARROW_UP);
            E.mode = 0;
            break;
    }
}

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
    size_t bufsize = 128;
    char *buf = malloc(bufsize);

    size_t buflen = 0;
    buf[0] = '\0';

    while (1) {
        editorSetStatusMessage(prompt, buf);
        editorRefreshScreen();

        int c = editorReadKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buflen != 0) buf[--buflen] = '\0';
        } else if (c == '\x1b') {
            editorSetStatusMessage("");
            if (callback) callback(buf, c);
            free(buf);
            return NULL;
        } else if (c == '\r') {
            if (buflen != 0) {
                editorSetStatusMessage("");
                if (callback) callback(buf, c);
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buflen == bufsize - 1) {
                bufsize *= 2;
                char *new_buf = realloc(buf, bufsize);
                if (new_buf == NULL) {
                    free(buf);
                    return NULL;
                }
                buf = new_buf;
            }
            buf[buflen++] = c;
            buf[buflen] = '\0';
        }

        if (callback) callback(buf, c);
    }
}

void editorMoveCursor(int key) {
    erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
    
    switch (key) {
        case ARROW_LEFT:
            if (E.cx != 0) {
                E.cx--;
            } else if (E.cy > 0) {
                E.cy--;
                E.cx = E.row[E.cy].size;
            }
            break;
        case ARROW_RIGHT:
            if (row && E.cx < row->size) {
                E.cx++;
            } else if (row && E.cx == row->size) {
                E.cy++;
                E.cx = 0;
            }
            break;
        case ARROW_UP:
            if (E.cy != 0) {
                E.cy--;
            }
            break;
        case ARROW_DOWN:
            if (E.cy < E.numrows) {
                E.cy++;
            }
            break;
    }

    row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
    int rowlen = row ? row->size : 0;
    if (E.cx > rowlen) {
        E.cx = rowlen;
    }
}

int findMatchingBracket(int row_idx, int col_idx) {
    if (row_idx >= E.numrows) return -1;
    erow *row = &E.row[row_idx];
    if (col_idx >= row->size) return -1;

    char c = row->chars[col_idx];
    char target;
    int direction;

    if (c == '(') { target = ')'; direction = 1; }
    else if (c == ')') { target = '('; direction = -1; }
    else if (c == '{') { target = '}'; direction = 1; }
    else if (c == '}') { target = '{'; direction = -1; }
    else return -1;

    int stack = 1;
    int r = row_idx, c_idx = col_idx + direction;

    while (r >= 0 && r < E.numrows) {
        erow *cur = &E.row[r];
        while (c_idx >= 0 && c_idx < cur->size) {
            if (cur->chars[c_idx] == c) stack++;
            else if (cur->chars[c_idx] == target) {
                stack--;
                if (stack == 0) return r * 1000 + c_idx; // Codificamos pos para brevedad
            }
            c_idx += direction;
        }
        r += direction;
        if (r >= 0 && r < E.numrows)
            c_idx = (direction > 0) ? 0 : E.row[r].size - 1;
    }
    return -1;
}

void editorProcessKeypress() {
    static int quit_times = KILO_QUIT_TIMES;
    int c = editorReadKey();

    switch (c) {
        case '\r':
            editorInsertNewline();
            break;

        case CTRL_KEY('q'):
            if (E.dirty && quit_times > 0) {
                editorSetStatusMessage("WARNING!!! File has unsaved changes. "
          "Press Ctrl-Q %d more times to quit.", quit_times);
                quit_times--;
                return;
            }
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;

        case CTRL_KEY('s'):
            editorSave();
            break;

        case HOME_KEY:
            E.cx = 0;
            break;

        case END_KEY:
            if (E.cy < E.numrows) {
                E.cx = E.row[E.cy].size;
            }
            break;

        case CTRL_KEY('f'):
            editorFind();
            break;

        case BACKSPACE:
        case CTRL_KEY('h'):
        case DEL_KEY:
            if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
            editorDelChar();
            break;

        case PAGE_UP:
        case PAGE_DOWN:
            {
                if (c == PAGE_UP) {
                    E.cy = E.rowoff;
                } else if (c == PAGE_DOWN) {
                    E.cy = E.rowoff + E.screenrows - 1;
                    if (E.cy > E.numrows) E.cy = E.numrows;
                }

                int times = E.screenrows;
                while (times--) {
                    editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
                }
            }
            break;

        case ARROW_UP:   // key movement cases
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            editorMoveCursor(c);
            break;

        case CTRL_KEY('l'):
        case '\x1b':
            E.mode = 1;
            break;

        default:
            editorInsertChar(c);
            break;

    }
    quit_times = KILO_QUIT_TIMES;
}

void editorNormalProcessKeypress() {
    static int quit_times = KILO_QUIT_TIMES;
    int c = editorReadKey();

    switch (c) {
        case CTRL_KEY('q'):
            if (E.dirty && quit_times > 0) {
                editorSetStatusMessage("WARNING!!! File has unsaved changes. "
          "Press Ctrl-Q %d more times to quit.", quit_times);
                quit_times--;
                return;
            }
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;

        case CTRL_KEY('s'):
            editorSave();
            break;

        case HOME_KEY:
            E.cx = 0;
            break;

        case END_KEY:
            if (E.cy < E.numrows) {
                E.cx = E.row[E.cy].size;
            }
            break;

        case CTRL_KEY('f'):
            editorFind();
            break;

        case PAGE_UP:
        case PAGE_DOWN:
            {
                if (c == PAGE_UP) {
                    E.cy = E.rowoff;
                } else if (c == PAGE_DOWN) {
                    E.cy = E.rowoff + E.screenrows - 1;
                    if (E.cy > E.numrows) E.cy = E.numrows;
                }

                int times = E.screenrows;
                while (times--) {
                    editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
                }
            }
            break;

        case ARROW_UP:   // key movement cases
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            editorMoveCursor(c);
            break;

        case 'i':
        case 'I':
        case 'a':
        case 'A':
        case 'o':
        case 'O':
            editorDoInsert(c);
            break;

        case 'w':
        case 'b':
        case '}':
        case '{':
            editorSpecialMovement(c);
            break;

        case 'p':
            editorSetStatusMessage("char at %d = %d", E.rx, E.row[E.cy].chars[E.cx]);
            break;

        default:
            editorMoveCursor(editorNormalMovement(c));
            break;

    }
    quit_times = KILO_QUIT_TIMES;

}

/**********\
  * init *
\**********/

void initEditor() {
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.rowoff = 0;
    E.coloff = 0;
    E.numrows = 0;
    E.row = NULL;
    E.dirty = 0;
    E.filename = NULL;
    E.statusmsg[0] = '\0';
    E.statusmsg_time = 0;
    E.syntax = NULL;
    E.mode = 1;

    if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
    E.screenrows -= 2;
}

int main(int argc, char *argv[]) {
    enableRawMode();
    initEditor();
    if (argc >= 2) {
        editorOpen(argv[1]);
    }

    editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    while (1) {
        editorRefreshScreen();
        if (E.mode == MODE_NORMAL) {
            editorNormalProcessKeypress();
        } else {
            editorProcessKeypress();
        }
    }
    return 0;
}

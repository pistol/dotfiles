"Custom VIM settings
set t_Co=256
set modelines=1

set visualbell

syntax on
"automatically wrap lines at 80 chars
"set tw=80

"enable xterm mouse support
set mouse=a
"drag visual selections and have them show up in your X copy buffer
set ttymouse=xterm2
"create tabs using spaces automatically
set expandtab
"number of spaces in a tab
set tabstop=4
set softtabstop=4
"indent spaces
set shiftwidth=4
set smartindent
set expandtab ts=4 sw=4 ai
"set lines=64 columns=80
set wildmode=longest,list

filetype on
"force colorscheme, apparently not always loaded by default
colorscheme leo

" Custom extensions to highlight
" slicc files
au BufNewFile,BufRead * colorscheme leo
au BufNewFile,BufRead *.sm set filetype=c
au BufNewFile,BufRead *.tig set filetype=tiger
au BufNewFile,BufRead *.lex set filetype=lex
au BufNewFile,BufRead *.grm set filetype=lex
au BufRead,BufNewFile *.smali set filetype=smali
au BufNewFile,BufRead *.dexdump set filetype=smali
au BufRead,BufNewFile *.dis setfiletype objdasm
if has ("autocmd")
    " File type detection. Indent based on filetype. Recommended.
    filetype plugin indent on
endif

"highlight search results
set hlsearch
set showmatch
set bs=indent,eol,start
set incsearch                   " live search while typing search expression
set nowrap                      " don't wrap visible lines
set shell=/bin/bash             " set the shell to be used
set ttyfast                     " for fast terminals - smoother (apparently)
set wildmenu                    " enhanced cmdline completion
set wildmode=longest:full,full  " cmdline completion mode settings

set history=1000

"GVIM/MacVim only options
if has("gui_running")
    "disable toolbars and scrollbars
    set guioptions=-tLR
    "set guifont=Menlo:h12
    set noantialias
    colorscheme leo
endif

"diff options
"default diff split to vertical
set diffopt=vertical

"vimdiff special colorscheme
"if &diff
"    colorscheme github
"else
"    "default color theme
"    colorscheme leo
"endif

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
"command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis

augroup AutoDiffUpdate
  au!
  autocmd InsertLeave * if &diff | diffupdate | let b:old_changedtick = b:changedtick | endif
  autocmd CursorHold *
        \ if &diff &&
        \    (!exists('b:old_changedtick') || b:old_changedtick != b:changedtick) |
        \   let b:old_changedtick = b:changedtick | diffupdate |
        \ endif
augroup END

nnoremap <silent> do do:let b:old_changedtick = b:changedtick<CR>
nnoremap <silent> dp dp<C-W>w:if &modifiable && &diff \| let b:old_changedtick = b:changedtick \| endif<CR><C-W>p

" If doing a diff. Upon writing changes to file, automatically update the
 " differences
au BufWritePost * if &diff == 1
au BufWritePost * :diffupdate
au BufWritePost * endif

nmap du :wincmd w<cr>:normal u<cr>:wincmd w<cr>

" Eclipse like editing, to move lines using arrow keys
function! MoveLineUp()
  call MoveLineOrVisualUp(".", "")
endfunction

function! MoveLineDown()
  call MoveLineOrVisualDown(".", "")
endfunction

function! MoveVisualUp()
  call MoveLineOrVisualUp("'<", "'<,'>")
  normal gv
endfunction

function! MoveVisualDown()
  call MoveLineOrVisualDown("'>", "'<,'>")
  normal gv
endfunction

function! MoveLineOrVisualUp(line_getter, range)
  let l_num = line(a:line_getter)
  if l_num - v:count1 - 1 < 0
    let move_arg = "0"
  else
    let move_arg = a:line_getter." -".(v:count1 + 1)
  endif
  call MoveLineOrVisualUpOrDown(a:range."move ".move_arg)
endfunction

function! MoveLineOrVisualDown(line_getter, range)
  let l_num = line(a:line_getter)
  if l_num + v:count1 > line("$")
    let move_arg = "$"
  else
    let move_arg = a:line_getter." +".v:count1
  endif
  call MoveLineOrVisualUpOrDown(a:range."move ".move_arg)
endfunction

function! MoveLineOrVisualUpOrDown(move_arg)
  let col_num = virtcol(".")
  execute "silent! ".a:move_arg
  execute "normal! ".col_num."|"
endfunction

nnoremap <silent> <C-S-Up> :<C-u>call MoveLineUp()<CR>
nnoremap <silent> <C-S-Down> :<C-u>call MoveLineDown()<CR>
inoremap <silent> <C-S-Up> <C-o>:<C-u>call MoveLineUp()<CR>
inoremap <silent> <C-S-Down> <C-o>:<C-u>call MoveLineDown()<CR>
vnoremap <silent> <C-S-Up> :<C-u>call MoveVisualUp()<CR>
vnoremap <silent> <C-S-Down> :<C-u>call MoveVisualDown()<CR>

" MAPPINGS
"shortcuts to quit current/all files
noremap <C-\> <Esc>:q<CR>
noremap <S-\> <Esc>:qa<CR>

"NERDtree shortcuts
"Toggle the tree window
noremap <C-n> :NERDTreeToggle<CR>

"CTRL-s for Save
"map <C-s> :w<CR>
"imap <c-s> <Esc>:w<CR>a 
noremap <C-s> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <C-O>:update<CR>

hi User1 guifg=#ff3300 guibg=#222222
hi User2 guifg=#0066ff guibg=#222222
hi User3 guifg=#ff66ff guibg=#222222
hi User4 guifg=#a0ee40 guibg=#222222
hi User5 guifg=#eeee40 guibg=#222222
hi User6 guifg=#ffffff guibg=#222222
hi User7 guibg=#111111

" statusline
" cf the default statusline: %<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P
" format markers:
"   %< truncation point
"   %n buffer number
"   %f relative path to file
"   %m modified flag [+] (modified), [-] (unmodifiable) or nothing
"   %r readonly flag [RO]
"   %y filetype [ruby]
"   %= split point for left and right justification
"   %-35. width specification
"   %l current line number
"   %L number of lines in buffer
"   %c current column number
"   %V current virtual column number (-n), if different from %c
"   %P percentage through buffer
"   %) end of width specification

set statusline=
set statusline +=[%n]\              "buffer number
set statusline +=%{&ff}             "file format
set statusline +=%y                 "file type
set statusline +=\ %<%F\            "full path
set statusline +=%m                 "modified flag
set statusline +=%r                 "readonly flag
set statusline +=%=[%3b]\ [0x%4B]\  "character under cursor
set statusline +=%4cc\              "column number
set statusline +=%5l                "current line
set statusline +=/%L\               "total lines
set statusline +=%3p%%\             "percent in file

"set statusline=%F%m%r%h%w\ %=[%{&ff}]\ %y\ [a=\%3b]\ [h=\%2B]\ [%4l/%L][%2p%%]

"show status line
set laststatus=2
"enable line numbers
set number

set directory^=$HOME/.vim_swap//   "put all swap files together in one place

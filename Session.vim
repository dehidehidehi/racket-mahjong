let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/Development/lisp/racket-mahjong
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
argglobal
%argdel
edit src/main.rkt
argglobal
balt .lvimrc
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 48 - ((32 * winheight(0) + 21) / 42)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 48
normal! 0
lcd ~/Development/lisp/racket-mahjong
tabnext 1
badd +0 ~/Development/lisp/racket-mahjong/src/main.rkt
badd +0 ~/Development/lisp/racket-mahjong/src/tests.rkt
badd +1 ~/Development/lisp/racket-mahjong/.lvimrc
badd +1 ~/Development/lisp/racket-mahjong/\!/usr/bin/zsh
badd +332 ~/.vimrc
badd +1 ~/Development/lisp/racket-mahjong/\!git\ ls-files\ \|\ entr\ -r\ raco\ test\ --drdr\ --make\ --quiet-program\ --heartbeat\ -p\ mahjong\ (2)
badd +1 ~/Development/lisp/racket-mahjong/autotest.sh
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOSc
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :

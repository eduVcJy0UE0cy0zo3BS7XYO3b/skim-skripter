modules = \
  modules/dom/canvas.scm \
  modules/dom/document.scm \
  modules/dom/element.scm \
  modules/dom/event.scm \
  modules/dom/image.scm \
  modules/dom/media.scm \
  modules/dom/window.scm \
  modules/ren-sexp/bg.scm \
  modules/ren-sexp/core.scm \
  modules/ren-sexp/music.scm \
  modules/ren-sexp/scene.scm \
  modules/ren-sexp/sprites.scm \
  modules/ren-sexp/text.scm \
  modules/ren-sexp/carret.scm \
  modules/ren-sexp/rssl.scm \
  modules/ren-sexp/keyboard.scm \
  modules/ren-sexp/draw.scm \
  modules/ren-sexp/update.scm \
  modules/ren-sexp/scene-utils.scm \
  modules/ren-sexp/debug-info.scm \
  modules/repl-environment.scm \
  modules/repl.scm \
  demo/assets.scm \
  modules/ren-sexp/utils.scm

game.wasm: game.scm $(modules)
	guild compile-wasm --async -g1 -L /gnu/store/q4648183qkl43ky8yp3qqikv63j67dw7-guile-goblins-0.15.1/share/guile/site/3.0 -L /gnu/store/9vm4izgsp0g007d82zgmrvg9zczkxjxj-profile/share/guile/3.0 -L modules -L . -o $@ $<

game.scm: game.org
	emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "game.org")'

serve: game.wasm
	guix shell python -- python3 -m http.server
	guile -c '((@ (hoot web-server) serve))'

bundle: game.wasm
	rm game.zip || true
	zip game.zip -r resources/ js-runtime/ game.js game.css game.wasm index.html favicon.ico

clean:
	rm -f game.wasm game.zip

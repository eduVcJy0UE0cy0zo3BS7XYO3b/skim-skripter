HOOT_LP=/gnu/store/9vm4izgsp0g007d82zgmrvg9zczkxjxj-profile/share/guile-hoot/0.6.0/lib:/gnu/store/9vm4izgsp0g007d82zgmrvg9zczkxjxj-profile/share/guile/3.0

GOBLINS_PATH=/gnu/store/q4648183qkl43ky8yp3qqikv63j67dw7-guile-goblins-0.15.1/share/guile/site/3.0

# GUILD=/gnu/store/h47jzdgll17fp17vwqjp70knc329z182-profile/bin/guild
GUILD=guild

modules = \
  modules/dom/canvas.scm \
  modules/dom/document.scm \
  modules/dom/element.scm \
  modules/dom/event.scm \
  modules/dom/fullscreen.scm \
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
  modules/ren-sexp/settings.scm \
  modules/ren-sexp/game-state.scm \
  modules/ren-sexp/menu.scm \
  modules/ren-sexp/menu-draw.scm \
  modules/ren-sexp/main-menu.scm \
  modules/ren-sexp/save-system.scm \
  modules/ren-sexp/save-menu.scm \
  modules/repl-environment.scm \
  modules/repl.scm \
  demo/assets.scm \
  modules/ren-sexp/utils.scm

version.scm:
	echo "(define-module (version)" > version.scm
	echo "  #:export (GAME_VERSION))" >> version.scm
	echo "" >> version.scm
	echo "(define GAME_VERSION \"v1.0-$(shell git rev-parse --short HEAD)\")" >> version.scm

game.wasm: version.scm game.scm $(modules)
	HOOT_LOAD_PATH=$(HOOT_LP) guix shell guile-next guile-hoot -- $(GUILD) compile-wasm --async -L $(GOBLINS_PATH)  -L modules -L . -o $@ game.scm

game.scm: game.org
	emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "game.org")'

serve: game.wasm
	guix shell python -- python3 -m http.server 8081
	guile -c '((@ (hoot web-server) serve))'

bundle: game.wasm
	rm game.zip || true
	zip game.zip -r resources/ reflect-js/ game.js wtf8.wasm reflect.wasm game.css game.wasm index.html favicon.ico

clean:
	rm -f game.wasm game.zip

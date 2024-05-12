modules = \
  modules/dom/canvas.scm \
  modules/dom/document.scm \
  modules/dom/element.scm \
  modules/dom/event.scm \
  modules/dom/image.scm \
  modules/dom/media.scm \
  modules/dom/window.scm \
  modules/math.scm \
  modules/math/rect.scm \
  modules/math/vector.scm

game.wasm: game.scm $(modules)
	guild compile-wasm -L modules -o $@ $<

serve: game.wasm
	guix shell python -- python3 -m http.server
	guile -c '((@ (hoot web-server) serve))'

bundle: game.wasm
	rm game.zip || true
	zip game.zip -r resources/ js-runtime/ game.js game.css game.wasm index.html favicon.ico

clean:
	rm -f game.wasm game.zip

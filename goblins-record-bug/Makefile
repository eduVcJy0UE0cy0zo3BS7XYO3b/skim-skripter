HOOT_LP=/gnu/store/y23m1wfm78jc5pr2vxh3fzdyx7xybl6r-profile/share/guile-hoot/0.6.1/lib:/gnu/store/y23m1wfm78jc5pr2vxh3fzdyx7xybl6r-profile/share/guile/3.0

GOBLINS_PATH=/gnu/store/q4648183qkl43ky8yp3qqikv63j67dw7-guile-goblins-0.15.1/share/guile/site/3.0

game.wasm: game.scm
	HOOT_LOAD_PATH=$(HOOT_LP) guild compile-wasm -L $(GOBLINS_PATH)  -L modules -L . -o $@ $<

serve: game.wasm
	guix shell python -- python3 -m http.server

1. make environment
```
guix pull
guix shell guile-hoot guile-goblins guile-next
```
2. fix Makefile, lines with paths on your computer
```
HOOT_LP=/gnu/store/y23m1wfm78jc5pr2vxh3fzdyx7xybl6r-profile/share/guile-hoot/0.6.1/lib:/gnu/store/y23m1wfm78jc5pr2vxh3fzdyx7xybl6r-profile/share/guile/3.0

GOBLINS_PATH=/gnu/store/q4648183qkl43ky8yp3qqikv63j67dw7-guile-goblins-0.15.1/share/guile/site/3.0
```

3. run `make serve`
make clean
make main
make output/$1.s
make output/$1.o
make output/$1.run
echo "⠀⠀⠀⠀⠀⠀⠀⠀⢐⢅⠀⠀⠪⡨⠀⠀⠀⠜⢌⢪⠀⡪⡊⠀⢑⢅⢀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⢀⠐⢅⢣⢂⠁⠔⠈⡀⠀⠨⢊⠆⠨⡂⠀⡠⡠⢐⢅⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠰⢀⢕⢅⢕⠄⠀⠅⠀⠀⠀⠡⠁⢌⠀⡐⡜⢔⢅⢆⠅⡂⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⢨⠢⡱⡨⡢⢣⠀⠀⠀⠀⠀⠀⠨⡂⢐⢜⠌⢂⢆⠅⡆⡃⢅⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠸⡨⡢⢪⠢⡃⡀⠀⠀⢨⢐⠨⢂⠕⡁⠀⠀⡗⢨⢊⡂⢅⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⡌⢂⠊⠂⠃⠕⢕⢡⠢⠨⡢⡪⢢⢃⠼⠀⡀⠔⠡⡱⡨⠂⡅⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⢀⠅⢠⠠⡠⡀⢀⠀⡠⡱⡑⡜⠔⠁⠂⠁⠀⠀⢕⠕⢜⠠⡂⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⢨⠂⢕⢑⠕⠔⠁⡤⢐⠨⢐⢕⢑⢄⠀⠀⠀⠀⡇⢕⠕⢜⢀⢀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⡕⢕⠑⣔⠀⠀⢟⠀⠀⡢⡱⡡⢣⠱⡡⡂⠠⡱⡑⡕⡱⢐⠡⢊⠔⡠⠠⡀⢀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⢀⢌⢎⢜⠜⡄⡠⠃⠀⠀⡪⢢⠱⡑⢕⢑⢌⢎⢢⢱⢕⠅⢅⠪⡐⢌⠢⢑⠌⡂⢅⠕
⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⡗⡕⡝⠈⠀⠀⠀⢀⠪⡢⢣⠱⡑⢕⢑⢌⢆⢣⡣⡑⡡⢊⠔⡡⢊⠔⡨⢂⠅⡊
⠀⠀⠀⠀⠀⠀⠀⠀⠀⢣⢱⢝⢜⢅⠀⠀⠀⠀⠑⠱⢑⢨⠐⡅⡣⡱⡁⢎⢎⢐⠌⡂⡊⠔⡡⢊⠔⡡⢊⠔
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⡸⡸⡪⡣⣂⠀⠀⠀⠀⠀⠀⠀⡪⢪⢘⢔⠸⡸⡱⡂⠅⡊⢔⠡⡂⢅⢊⠔⡡⢊
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠱⡱⡱⡹⡸⡀⠀⠀⠀⠀⠀⠀⡊⢆⢃⢆⠣⡕⣕⠅⠨⢂⠅⡊⢔⠡⢂⠕⡐⡡
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢨⢪⡺⡸⡱⡀⠀⠀⡄⢆⠣⡪⡘⡌⠆⡱⡹⡸⡀⠊⡐⡡⢊⠔⡡⠡⡊⠔⡐
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⢏⢎⢎⢎⢞⢜⠀⢌⢎⡢⡣⡪⠊⠈⠀⡪⡪⡂⠅⠠⢐⠌⡂⡪⢐⠡⢊⠌⡢
⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⢯⢪⢪⢪⢪⢪⢣⢣⠀⠁⠊⠀⠀⠀⠀⠀⢱⢱⠁⢀⠂⢀⠪⡐⢌⠢⠡⡑⠌⠔
⠀⠀⠀⠀⠀⠀⠀⠀⢠⡯⡣⡣⡣⡣⡣⡱⢑⢕⢥⠀⠀⠀⠀⠀⠀⠀⡘⠬⢠⠂⠀⡂⠨⡐⢅⢊⠌⡢⢑⠡
⠀⠀⠀⠀⠀⠀⠀⢠⡟⡜⡬⡪⡪⡪⡪⡪⡐⡠⡑⡝⡔⡀⡀⡀⡠⡰⡑⠁⢸⢐⠀⠱⡈⠔⡡⢂⠕⡐⡡⢊
⠀⠀⠀⠀⠀⠀⢠⡿⡱⡱⡱⡸⡸⡨⡪⡪⡢⠐⡰⠈⢌⠪⢪⢘⠌⠂⠀⠀⠂⠰⠀⠁⢢⠑⡐⢅⢊⠔⡨⢂"
./output/$1.run
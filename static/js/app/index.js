var canvas;
        var context;
        var isWhite = false;//设置是否该轮到白棋
        var isWell = false;//设置该局棋盘是否赢了，如果赢了就不能再走了
        var img_b = new Image();
        img_b.src = "images/b.png";//白棋图片
        var img_w = new Image();
        img_w.src = "images/w.png";//黑棋图片
        var background = new Image();
        background.src = "images/a.jpg";
 
        var chessData = new Array(15);//这个为棋盘的二维数组用来保存棋盘信息，初始化0为没有走过的，1为白棋走的，2为黑棋走的
        for (var x = 0; x < 15; x++) {
            chessData[x] = new Array(15);
            for (var y = 0; y < 15; y++) {
                chessData[x][y] = 0;
            }
        }
 
        function drawRect() {//页面加载完毕调用函数，初始化棋盘
            canvas = document.getElementById("canvas");
            context = canvas.getContext("2d");

            context.drawImage(background,0,0,640,640);
 
            for (var i = 0; i <= 640; i += 40) {//绘制棋盘的线
                context.beginPath();
                context.moveTo(0, i);
                context.lineTo(640, i);
                context.closePath();
                context.stroke();
 
                context.beginPath();
                context.moveTo(i, 0);
                context.lineTo(i, 640);
                context.closePath();
                context.stroke();
            }
        }
        function play(e) {//鼠标点击时发生
            var x = parseInt((e.clientX - $('#canvas').offset().left - 20) / 40); 
            var y = parseInt((e.clientY - $('#canvas').offset().top - 20) / 40);
 
            if (chessData[x][y] != 0) {
                return;
            }
 
            if (isWhite) {
                isWhite = false;
                drawChess(1, x, y);
            }
            else {
                isWhite = true;
                drawChess(2, x, y);
            }
 
        }
        function drawChess(chess, x, y) {//参数为，棋（1为白棋，2为黑棋），数组位置
            if (isWell == true) {
                alert("已经结束了，如果需要重新玩，请刷新");
                return;
            }
            if (x >= 0 && x < 15 && y >= 0 && y < 15) {
                if (chess == 1) {
                    context.drawImage(img_w, x * 40 + 20, y * 40 + 20);//绘制白棋
                    chessData[x][y] = 1;
                }
                else {
                    context.drawImage(img_b, x * 40 + 20, y * 40 + 20);
                    chessData[x][y] = 2;
                }
            }
        }
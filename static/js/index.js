var canvas;
var context;
var isWhite = false;//设置是否该轮到白棋
var img_b = new Image();
img_b.src = "images/b.png";//白棋图片
var img_w = new Image();
img_w.src = "images/w.png";//黑棋图片
var background = new Image();
background.src = "images/a.jpg";
var chessData;

var w_x;
var w_y;
var state;

function init_t() {
    isWhite = false;
    isWell = false;
    chessData = new Array(15);
        for (var x = 0; x < 15; x++) {
            chessData[x] = new Array(15);
            for (var y = 0; y < 15; y++) {
                chessData[x][y] = 0;
            }
        }
    drawRect();
}

function init () {
    $.getJSON("init.json", function(data){
        init_t();
    });
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
            if (isWell) {
                return;
            }
            var x = parseInt((e.clientX - $('#canvas').offset().left - 20) / 40); 
            var y = parseInt((e.clientY - $('#canvas').offset().top - 20) / 40);
 
            if (chessData[x][y] != 0) {
                return;
            }
 
            if (!isWhite) {
                isWhite = true;
                drawChess(2, x, y);
                $.post("user.json",{x: y, y: x}, function (data) {
                    state = data["state"];
                    w_x = data["x"];
                    w_y = data["y"];
                    if (state === 0) {
                        isWhite = false;
                        drawChess(1, w_x, w_y);
                    } else if (state === 2) {
                        alert("You Win!");
                        init();
                    } else if (state === 1) {
                        isWhite = false;
                        drawChess(1, w_x, w_y);
                        alert("You Lose!");
                        init();
                    } else {
                        alert("error");
                    }
                },"json")
            }
        }
        
        function drawChess(chess, x, y) {
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
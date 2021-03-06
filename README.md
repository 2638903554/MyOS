﻿# MyOS
手动写汇编系统
[我的博客](http://www.tonlyshy.cn)
这是这学期操作系统作业的一部分，代码很大部分来自老师，这里我只是进行学习以及扩展。
而老师代码参考了于渊的Oranges
写这些代码可以更深入的了解操作系统、文件系统（这里用的是最为简单的FAT12）

2016/5/28：暂时不更新了，因为考虑到期末大作业...考完了再更

2016/9/10：完整代码
下面是实验部分报告
ps:MarkDown我还不会用，而且实验报告本来是word,字实在太多了不想改了=_=
基于1.44M  FAT12软盘的命令行程序（能显示汉字）
其中命令行工作在 VGA 640*480的图形模式下 对应文本显示80*30
（进入方法：用到BIOS 10h的0号功能，参数al=12h）
　　下面是内存地址分布：

1. 命令行
[1] 汉字显示（包括加载HZK16字库、Unicode码转GB2312码再算出偏移量、打点显示）
[2] 实现cd命令，自由切换目录
[3] 实现mkdir命令，创建子目录（注：在子目录创建条目有限制：子目录没有空间放更多文件条目时无法创建）
[4] 实现rename命令，能重命名文件和目录
[5] 加入锁屏功能，锁屏界面密码输入显示*，不会显示明文（相关函数getstrln0）
[6] 可修改密码，同样密码输入显示*，不会显示明文
[7] 开机动画start
[8] 可重启
[9] Dir和Ls能显示中文（限制：fat12中，中文条目不能完整显示：第一个字符不能显示，8+3的特性加上汉字占2字节，最后最多能显示后3个汉字）
[10] 实现21h中文显示中断 ah=42h int 21h
[11] 实现DispStr_Chinese 汉字显示子函数（调用方式与DispStr英文显示一致）
[12] DrawLine、drawPixel、drawRectangle图形辅助函数的实现
[13] WriteSec写扇区、getEmptyFatEntry获取空闲簇、setEmptyFatEntry设置空闲簇为占用簇 文件写入辅助函数实现
[14] Readmemmory debug工具，直接读取显示内存XXXX:YYYY处的16字节
[15] 汉化 提示串均以汉字显示（包括所有子命令）
[16] 实时时钟中断显示，并在内部命令以及外部命令时 恢复原BIOS时钟入口，以免影响程序运行（Store_dc、Shut_dc）
开发平台与工具
　　虚拟机（VMware和Bochs）、编辑器（Notepad++）、汇编器（NASM）、二进制编辑器（WinHex）、软盘写入程序（DiskWriter）、磁盘映像件（WinImage）、生成com


注：与读扇区相关的代码均基于ls或者exec的源码修改
食用说明：
1) 主程序
* 逻辑框图

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-1-1.png)

图3-1-1 cmd主要流程（放大可看清）开机动画在Int213dh中调用

* 运行界面

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-1-2.png)

图3-1-2 开机动画，带进度条（这段时间在加载字库，每读60个扇区（30kb）进度条更新一次，并更换颜色，9次大概260kb，即字库文件的大小）

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-1-3.png)

图3-1-3 字库加载完成立即达到锁屏界面（仿ubuntu界面），输入密码登录
使用了自己写的图形辅助函数画这个界面，以及DispStr_Chinese显示汉字

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-1-4.png)

图3-1-4 源码中汉字定义方法（如何使用在下面单独说明）

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-1-5.png)

图3-1-5 密码错误会提示

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-1-6.png)

图3-1-6 密码太长提示

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-1-7.png)

图3-1-7 密码输入正确后就可使用啦
注：其它内部命令单独拿出来解释说明
2) cd改变目录（命令）
函数cdToDir
* 逻辑框图

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-2-1.png)

图3-2-1 cd的流程图，主要任务是获得下一目录的扇区信息，确保ls程序以及exec搜索扇区时在正确的目录

* 运行界面

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-2-2.png)

图3-2-2 cd 正确的目录会修改当前目录，cd 不存在的目录会提示没有那个文件或目录（每次只能跳一级目录）
注:在FAT12中 . 和..也是条目，分别对应当前目录以及上一级目录，所以cd . 、cd ..这样的命令也可以支持，只是要作一点检测（根目录扇区号以及文件条目中的首簇号是不同的，根目录扇区号存的是绝对位置，文件条目中是相对数据区起始扇区的偏移，所以有时要加1fh，有时要减1fh，比较繁琐（对应公式 （FAT中扇区号-2）*200h+4200h）

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-2-3.png)

图3-2-3 cd . cd..均作了检测以及对应跳转
另外模仿MSDOS，加入了任意子目录cd \ 就可跳回根目录的检测

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-2-4.png)

图3-2-4 cd \ 直接跳回根目录（初始化为根目录扇区19，扇区数14并修改提示串就好）

* 部分代码截图
3) 加载字库（非命令）
函数int213dh：负责将HZK16加载到起始地址为30000h的地方，供开机后使用（所以这部分内存限制没有给cmd或者外部程序使用）
* 逻辑框图

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-3-1.png)

图3-3-1 加载字库的流程
* 运行界面

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-3-2.png)

图3-3-2 加载字库时间比较长，所以引入了动画
注：每读完一个扇区都会调用start动画函数，但是不是每次调用都会更新动画，动画函数里有个计数器，到达40才回更新一次并清0。大概会动9次，读60个扇区，每个扇区0.5k，40*0.5*9=270kb，可以验证它读的是HZK16。
![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-3-3.png)

图3-3-3 HZK16字库的资料

* 部分代码截图

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-3-4.png)

图3-3-4 部分 加载字库用到的代码
4) Int 21h ah=42h显示中文的中断（非命令）
* 逻辑
与DispStr_Chinese相同
DispStr_Chinese:;
在当前位置显示汉字 串地址=BP 串长=CX  颜色属性=AL 行号DH 列号DL

注：计算字在字库中的偏移的代码由 林林凡以及李粤琛写，我负责将其整合到cmd中，但是由于源代码用到了int 21h 中读取文件的中断，但是cmd没有21号中断，只好自己写一个加载字库的程序，放弃中断调用，默认将字库读到3000:0000的位置，每个字的字模占32字节，由计算程序算出偏移地址，我将内存中偏移段地址+3000:偏移地址处的32字节读给显示程序即可。
* 部分代码

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/4-1-1.png)

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/4-2-1.png)

在原本cmd的21h中断中，加入对ah的检测

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/4-3-1.png)


注：这里在设置一些必要的参数，之后会调用计算以及显示函数HZK16_test（内部命令CHINESE就是直接调用这个函数）

5) Getstrln改进 退格检测（略）
有退格检测，是之前平时作业的选做部分，直接搬运过来了
6) LS程序改进
为什么ls需要改进？因为它默认只会读根目录SectorNoOfRootDirectory，这里修改为SectorNoOfCurrentDirectory，这个参数在Cd中得到维护，每次cd后会修改它，保证ls以及其它文件操作正常进行
* 部分代码

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/6-1-1.png)

注：入代码注释，根目录的下一扇区直接加512byte就好，但是对于子目录，他不一定是连续存储的，还是要根据FAT项来确定下一扇区号，并且坑爹的相对扇区与绝对扇区，每次都要 减掉1fh
甚至加入了中文检测

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/6-2-1.png)

演示效果：（fat12的限制，第一个汉字显示不了（因为第一个不是汉字））

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-6-1.png)

图3-6-1 ls改进的汉字显示以及支持子目录ls

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-6-2.png)

图3-6-2 winImage查看的目录结构
7) 重启(命令)
很简单的直接调用int19h中断
* 全部代码

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/7-1-1.png)

8) Mkdir 创建子目录
与cd结合就非常完美了，这两个也是大作业中耗时最长最有分量的
Cd的存在，以及读扇区的改进，让mkdir支持了任意目录下的创建文件夹操作（这点和ls以及之后的rename一样）
* 逻辑框图

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-8-1.png)

图3-8-1 mkdir流程（核心其实是写FAT表以及写扇区、如何利用时间、扇区信息构造文件条目）
* 运行界面

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-8-2.png)

图3-8-2 创建子目录，配合cd可跳至子目录，ls也正常，看到初始目录有.和..两个文件条目，时间也为当前时间，证明mkdir正确

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-8-3.png)

图3-8-3 已有s2的情况下 mkdir s2就会报错，算是一种保护机制
注：没有太多命令合法性检测，乱输可能会出问题

* 部分代码
Mkdir涉及写扇区，为了方便加入新的辅助函数
WriteSec 与ReadSec相呼应，直接完成把内存中ES：BX位置的数据写到起始扇区号为AX的cl个扇区中

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/8-1-1.png)

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/8-2-1.png)

实际上对比readsec的代码，我就改了调用中断的功能号（2->3），改动难度不大，难在接下来写到哪里以及如何找到FAT项
于是又写了操作FAT项的两个辅助函数，getEmptyFatEntry  setEmptyFatEntry，听名字就知道，他们一个负责找到空的FAT项，找到后在AX中返回，setEmptyFatEntry 利用AX传来的项填入FFF（即这个簇被占用，即将放入子目录的两个条目.和..）

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/8-3-1.png)

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/8-4-1.png)

如何构造条目串？

如上图，先预留了InitialDirBuf 合计512Byte空间，等待写入，获得时间以及扇区信息后先写到这里，再由WriteSec写入即可

而对于当前目录如何增加子目录条目，因为当前目录扇区还保留在缓冲区里，直接去缓冲区修改即可再调用WriteSec写入
9) 内部命令支持带参数
我们刚才已经用过了，其实只是在带参数的内部命令里面，把getstrln中的buf 中命令串长+1之后的字串取出来就行了
以Cd为例，直接跳过3个字节，就能取出之后的字串并复制到DirBuf

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/8-5-1.png)

获取时间是由子程序实现的  ：CountTimeDate和CountTimeDate_Initial

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/8-6-1.png)

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/8-7-1.png)

10) Rename 重命名文件、目录
与mkdir一样涉及写扇区，比起mkdir相对要容易一点，难在将命令转为文件名，比如123.txt的格式要转为123       txt的8+3的格式，而目录 First 这种 直接FIRST    8+3就好了
* 逻辑框图

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/8-8-1.png)

图3-10-1 rename重命名流程
* 运行界面

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-10-2.png)

图3-10-2对目录的改动

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-10-3.png)

图3-10-3 对文件的改动
注:op1.com是之前作业中的外部程序，在图形模式也是能正常跑的，改为op2.com后可以看到op2能用

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-10-4.png)

图3-10-4 对文件的改动续，以及出错检测
注:op1已经不存在，再ls看看目录，可以看到对应位置上的是op2。
* 部分代码

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-10-5.png)

实际上和mkdir代码是类似的
11) 各种辅助函数（画图、debug用）
Hex2ascii 显示al的2为16进制数字
drawLine:  画线    ;起始坐标 BX  结束坐标 DX  不变坐标 AX  CX划线方向 0水平 1竖直
drawRectangle: ;画矩形 
画图函数用法:

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/11-1-1.png)

a b c 三点可以确定一个矩形，将坐标（640*480 要对应行列，屏幕坐标是从左上角开始的，这点要注意）
DispStr_Chinese用法
类似DispStr

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/11-2-1.png)


可以看出调用方法一致，这样做起因就是图方便

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/11-3-1.png)

而汉字字长要注意，因为一个汉字占两字节，所以要除以二
汉字中断用法

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/11-4-1.png)

这是一个独立的程序，放在cmd里面即可调用int 21h ah=42h号中断
这个是模仿int10h号中断的ah=13h，也是
BP 串偏移地址
CX 串长
DH 行号（转换为文本模式下（行距16像素点，列距8像素点，总640*480 转化为文本就是80*30））
DL 列号（注意汉字列占两格，所以dl=4 实际上是8）
BL 颜色属性

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-11-1.png)

图3-11-1 htest跑动效果
CopyPassword

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/11-5-1.png)

passwordStr_temp1
passwordStr_temp2 分别用于保存两次输入的密码
Getstrln0
以下是简略的改动（回显显示*号）

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/11-6-1.png)

12) CPASS、CUSER以及锁屏
Cpass较复杂一点，和锁屏程序类似（基于锁屏代码改动的）
核心是基于getstrln修改的getstrln0，省去自己写获取命令串的时间
CUSER做的比较仓促，改变锁屏界面的用户名
LOCK命令就是跳到开机时进入的SignIn子函数，只是自动锁屏没有实现，所以加了一个手动锁屏
* 逻辑框图（以CPASS为例）

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/12-1-1.png)

* 运行界面
CPASS

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-12-1.png)

图3-12-1 两次输入不一致会重新要求输入（密码回显以*号代替）

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-12-2.png)

图3-12-2 输入正确自动跳至锁屏（类似qq密码改动后要求重新登录）
CUSER  LiaoDaPao 实际上就是把后面的几位字串字节覆盖到原用户名串

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-12-3.png)

图3-12-3 改变名字为LiaoDaPao

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-12-4.png)

图3-12-4 进入锁屏查看情况
* 部分代码
CPASS：流程与锁屏差不多，关键是getstrln0获得数据后，CopyPassword将buf内的字串复制出来，给changepassword使用

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/12-2-1.png)

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/12-3-1.png)


LOCK:初始化内部命令入口时就可看到,LOCK直接跳进了SignIn

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/12-4-1.png)


13) READMEM
读取 XXXX:YYYY处16字节的内存信息
实现比较容易，演示一下汉字加载是否正确
根据HZK16提供的起始地址计算方法程序我们算出 【目】 字字模的起始地址为19F00H
00H,10H,1FH,F8H,10H,10H,10H,10H,
10H,10H,1FH,F0H,10H,10H,10H,10H,
10H,10H,1FH,F0H,10H,10H,10H,10H,
10H,10H,1FH,F0H,10H,10H,00H,00H
（32byte 字模数据）

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-13-2.png)

图3-13-2 HZK16资料中提供的计算程序

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-13-3.png)

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/3-13-4.png)

可以看到读出来是正确的，可以尝试验证其正确性

中文显示算法
·16*16点阵汉字显示原理
 汉字内码： ASCII表的高128个很少用到的数值以两个为一组来表示汉字，即汉字的内码，而剩下的低128位则留给英文字符使用，即英文的内码。所以每个汉字在计算机中保存的是2个字节的内码。
 汉字字模：所有的数据都是以0和1保存的.
 比如中文的“我”在字模在HZK16字库中是这样记载的

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/13-1-1.png)

·HZK16汉字字库使用方法
　　HZK16汉字字库是一个存储了汉字字模的文件，可以通过中文的内码计算其字模的起始位置。
　　假设内码为ABCD(AB在前为区码，CD在后为位码)
	则起始位置ADDRESS= [(AB-0A1H)*94(十进制)+（CD-0A1H）*32(十进制)]
·画字算法
* 存储汉字内码，打开字库文件，计算获得该汉字在字库中字模的起始位置
* 设置进入640*480/16色显示模式，利用bios中断0CH功能进行像素点描点汉字
* 每个汉字：读两个字节的字模数据于ax寄存器中，每一行循环16次左移一位根据移出的这位为0/1判断画点还是不画，16次后进入下一行循环16次则一个汉字描绘完成
显示流程：

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/13-2-1.png)	

运行实例：

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/13-3-1.png)

注：汉字显示对代码的编码格式有要求，我们的公式是根据GB2312编码来的，而一般notepad++ 是Unicode编码，这一点要特别注意。

设置方法:

![image](https://github.com/SteiensGate/MyOS/raw/master/picture/13-4-1.png)
 


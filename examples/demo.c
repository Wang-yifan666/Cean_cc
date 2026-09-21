/* cean 目前支持的语言子集示例
 *
 * 运行：
 *   lake exe cean examples/demo.c
 *
 * 期望输出（变量终值）：
 *   x = 999
 *   y = 24
 *   i = 10
 */

int main() {
  int x = 1 + 2 * 3;   /* 乘法优先 => 7 */
  int y = 0;
  int i = x;

  /* 累加 7 + 8 + 9 = 24 */
  while (i < 10) {
    y = y + i;
    i = i + 1;
  }

  /* 条件成立，走 then 分支 */
  if (y == 24 && x < 10) {
    x = 999;
  } else {
    x = 0;
  }
}

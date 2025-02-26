## a1

R_Vcalc = 200O
R_V = 220O

## a2

1. widerstand siehe a1

2. programm kann gestoppt werden durch interrupt. globale variable speichert, welche led gerade leuchtet. taster-interrupt checkt ob die richtige leuchtet.

## 3.1

1 = or
2 = nand
3 = xor

## 4

4.1.1a) (4 _ 10 + 7 _ 1) _ 10^3 = 47kO, +/- 5%
4.1.1b) (3 _ 10 + 9) _ 10^5 = 3.9MO, +/- 5%
4.1.1c) (6 _ 10 + 8) \* 100 = 6.8kO, +- 10% (Annahme Silber, wenn Grau dann 0.05%)

4.1.2.a)
U*a = U_e * R*2/(R_1 + R_2)
3V = 5V * (R_2)/(R_1 + R_2)
R_2/(R_1 + R_2) = 3/5 = 0.6

somit

R_2/(10 + R_2) = 3/5
R_2 = 6 + 0.6R_2
0.4R_2 = 6
R_2 = 15kO

4.1.2.b)

siehe a)

R_x = 2.5O

4.1.3a)

es gilt U_C(t) = U_q(1 - e^(-t/tau))

0.63 = 1 - e^-0.1/tau
tau = 0.1s

wegen C = tau/R also C = 10 microF

4.1.3b) gesucht ist t mit 0.99 = 1 - e^-t/tau
t = 461ms

5.1.1a)

0V
U/4
U/2
3U/4

sum_i=1...n(U_i/2^i) \* U_ref

| Voltage (V) | Decimal | Hexadecimal |
| ----------- | ------- | ----------- |
| 2.5         | 512     | 0x200       |
| 3.3         | 675     | 0x2A3       |
| 4.0         | 818     | 0x332       |
| 4.5         | 921     | 0x399       |
| 4.9         | 1003    | 0x3EB       |
| 5.0         | 1023    | 0x3FF       |
| 4.9         | 1003    | 0x3EB       |
| 4.5         | 921     | 0x399       |
| 4.0         | 818     | 0x332       |
| 3.3         | 675     | 0x2A3       |
| 2.5         | 512     | 0x200       |
| 1.7         | 348     | 0x15C       |
| 1.0         | 204     | 0x0CC       |
| 0.5         | 102     | 0x066       |
| 0.1         | 20      | 0x014       |
| 0.0         | 0       | 0x000       |
| 0.1         | 20      | 0x014       |
| 0.5         | 102     | 0x066       |
| 1.0         | 204     | 0x0CC       |
| 1.7         | 348     | 0x15C       |

b)

T = 1/f
bit pattern duration = T/20

first value = (1/f/20) \* 10^6 = 190,8396946565 microseconds

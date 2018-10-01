# Space Shooter

In dieser Übung werden wir ein relativ fortgeschrittenes Computerspiel in C entwickeln, welches im Terminalemulator -- unter Windows standardmäßig, CMD -- läuft.Wir werden im Terminal mithilfe von spezielle control codes (Ketten von escape characters) das gesamte Spiel zeichnen und durch Tastendrücke auf dem Keyboard kontrollieren.

Diese Übung setzt einfache Programmierkentnisse voraus, so zum Beispiel die Konzepte von Bedingungen, Schleifen und Variablen. Die Programmiersprache C ist grundsätzlich eine sehr simple, und viele andere Sprachen (z.B. JavaScript, Java, C#) sind ihr oberflächlich sehr ähnlich -- Vorkentnisse in einer solcher Sprachen kann hilfreich sein.

## Spielablauf

![Screenshot während des pausierten Spiels](https://i.imgur.com/4TgnLB8.png)

Gegner, hier 4x4 Rechtecke, fliegen von oben nach unten und müssen vom Spieler elimiert abgeschossen werden. Wie im Luftkampf zwischen kleineren Fliegern üblich, reicht ein einziger Treffer um die feindlichen Rechtecke auszuschalten. Das Spiel läuft endlos, bis eines der gegnerischen Objekte das untere Ende des Bildschirms erreicht, wobei jeder Abschuss einen Punkt bringt -- das Ziel ist die Anhäufung möglichst vieler Punkte.

Der Spieler kontrolliert sein Raumschiff vertikal und horizontal (also von links nach rechts, und von oben nach unten) wie in Computerspielen üblich mit den WASD Tasten, und kann mit drücken der Space-Taste Geschosse aubfeuern.

![Screenshot zu Beginn des Spiels](https://i.imgur.com/p0jb5PI.png)

Dieser screenshot nimmt indirekt ein Implementationsdetail vorweg, die Bewegunspräzision. Um schnelle und flexible Bewegung zu erlauben, bewegt sich das Raumschiff normalerweise in Viererschritten: ein drücken der Taste 'A' == vier Einheiten nach links. Da dies aber meist nicht ausreichend für präzise Schüsse ist, können Bewegungen in Einserschritten durch halten der Shift-Taste eingeschaltet werden.

Wenn du möchtest, kannst du dir das ausprogrammierte Spiel [hier](https://github.com/LW2904/vt-space/releases) herunterladen, um den Spielablauf genauer zu sehen.

## Vorwissen

__VT100 Terminal Control Escape Sequences__ (kurz: VT100 codes) erlauben uns innerhalb eines Terminals bzw. Terminalemulators z.B. die Curserposition zu ändern, oder den Bildschirm zu löschen.

Escape sequences werden, wie escape characters, zwar abgesendet (z.B. via `printf` in C oder `process.stdout.send` in NodeJS) aber vom Terminal nicht genau so ausgegeben. Der escape character `\n` gibt beispielsweise an, dass ein Text von einer Zeile in die nächste übergehen soll. Eine escape sequence ist ganz einfach eine Kette von Zeichen welche interpretiert, also nicht als solche ausgegeben, werden -- so löscht `\e[2J` z.B. den sichtbaren Bildschirm.

Eine Liste nützlicher escape sequences ist [hier](http://www.termsys.demon.co.uk/vtansi.htm) zu finden, näheres zu ihrer Verwendung folgt.

```C
#include <stdio.h>

int main()
{
	printf("\e[2J");
}
```

Im obigen Beispiel sind zwei "bad-practises" enthalten, also schlechter Code-Stil:
- `\e` ist nicht standardisiert (also nicht garantiert was wir erwarten), und...
- `printf("\e[2J")` ist "Magie" -- es ist nicht direkt ersichtlich was dieses Stück code macht

Eine schönere Lösung wäre daher

```C
#include <stdio.h>

/* 0x1B is the ASCII "escape" character. */
#define ESC 0x1B

void clear_screen();

int main()
{
	clear_screen();
}

void clear_screen()
{
	printf("%c[2J", ESC);
}
```

Die Funktion `clear_screen` abstrahiert in diesem Fall den "magischen" Teil, und ihre Wirkung ist klar. 

Verstanden zu haben wie escape characters und sequences, und damit vt100 codes, zu verwenden sind, ist der Schlüssel zu dieser Übung. Wenn dir hier etwas unklar ist, solltest du dich noch ein wenig mit der obenstehenden Sektion beschäftigen, oder eine/n Mentor/in danach fragen.

## Entwicklungsschritte

Bei größeren Projekten ist es immer hilfreich, die Entwicklung auf kleinere Schritte herunterzubrechen. In diesem Fall könnte das in etwa wie folgend aussehen:

- Raumschiff kann bewegt werden und wird korrekt gezeichnet
	- Verschiedene Präzisionsstufen sind, wenn angebracht, implementiert
	- Kann nicht aus dem Terminalemulator heraus bewegt werden
- Projektile können abgefeuert werden
	- Verschwinden nachdem sie das obere Ende erreicht haben
	- Bewegen sich in einer angebrachten Geschwindigkeit
	- Maximale Anzahl ist auf ein vernünftiges Limit begrenzt
- Gegner erscheinen and bewegen sich
	- Verschwinden wenn sie von einem Projektil getroffen werden
	- Langsame Schwierigkeitserhöhung durch allmähliche Erhöhung der Erscheinungsfrequenz
	- Spiel wird nach erreichen des unteren Endes beendet

## Projektsetup

TODO: Projektsetup Beschreibung -- MinGW oder VS? Mentor oder selbstständig?
TODO: Projektstruktur (build script?)

## Bewegung des Raumschiffes


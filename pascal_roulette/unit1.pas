unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    greenRadio: TRadioButton;
    numberEdit: TEdit;
    wagerEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    balanceLabel: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    wlLabel: TLabel;
    Label6: TLabel;
    rolledLabel: TLabel;
    resultLabel: TLabel;
    redRadio: TRadioButton;
    blackRadio: TRadioButton;
    oddRadio: TRadioButton;
    evenRadio: TRadioButton;
    numberRadio: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);

    // Display values.
    procedure displayValues();

    // Update values.
    procedure lost(wager: integer);
    procedure won(wager: integer; multiplier: integer);
  private

  public

  end;

var
  Form1: TForm1;
  wins, losses, balance: integer;

const
  // Standard french roulette wheel, define explicitly to be able to get color.
  // 0 = green, odd indices = red, even in. = black
  wheel: array [0..36] of integer = (0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27,
    13, 36, 11, 30, 8, 23, 10, 5, 24, 16, 33, 1, 20, 14, 31, 9, 22, 18, 29, 7,
    28, 12, 35, 3, 26);

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.displayValues();
begin
  balanceLabel.Caption := IntToStr(balance);
  wlLabel.Caption := IntToStr(wins) + '/' + IntToStr(losses);
end;

procedure TForm1.won(wager: integer; multiplier: integer);
begin
  resultLabel.Caption := 'Won.';

  Inc(wins);

  balance := balance + wager * multiplier;

  displayValues();
end;

procedure TForm1.lost(wager: integer);
begin
  resultLabel.Caption := 'Lost.';

  Inc(losses);

  balance := balance - wager;

  displayValues();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize variables.
  wins := 0;
  losses := 0;
  balance := 1000;

  // Display initial values.
  displayValues();
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  rolledColor: string;
  index, rolled, wager: integer;
begin
  // Verify that required values are set.
  if (wagerEdit.Text = '') or ((numberRadio.Checked) and (numberEdit.Text = '')) then
  begin
    ShowMessage('Please verify your input.');
    Exit;
  end;

  wager := StrToInt(wagerEdit.Text);

  // Spin the wheel.
  randomize;
  index := random(36);
  rolled := wheel[index];

  if (index = 0) then
    rolledColor := 'green'
  else if ((index mod 2) = 1) then
    rolledColor := 'red'
  else
    rolledColor := 'black';

  rolledLabel.Caption := IntToStr(rolled) + ' (' + rolledColor + ')';

  if (numberRadio.Checked) and (StrToInt(numberEdit.Text) = rolled) then
  begin
    won(wager, 35);
    Exit;
  end;

  if (greenRadio.Checked) and (rolled = 0) then
  begin
    won(wager, 35);
    Exit;
  end;

  if (redRadio.Checked) and ((index mod 2) = 1) then
  begin
    won(wager, 1);
    Exit;
  end;

  if (blackRadio.Checked) and ((index mod 2) = 0) then
  begin
    won(wager, 1);
    Exit;
  end;

  if (oddRadio.Checked) and ((rolled mod 2) = 1) then
  begin
    won(wager, 1);
    Exit;
  end;

  if (redRadio.Checked) and ((rolled mod 2) = 1) then
  begin
    won(wager, 1);
    Exit;
  end;

  lost(wager);
end;

end.

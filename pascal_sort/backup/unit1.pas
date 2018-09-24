unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  Numbers: array [0..20000] of Integer;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
     // StringGrid1.RowCount := length(Numbers);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
     randomize();

     for i := 0 to length(Numbers) - 1 do begin
         Numbers[i] := random(100000);

         StringGrid1.cells[i mod 15, i div 15] := IntToStr(Numbers[i]);
     end;
end;

procedure WriteToTable(Arr: array of Integer);
var
  i: Integer;
begin
     for i := 0 to length(Arr) - 1 do begin
         StringGrid1.cells[i mod 15, i div 15] := IntToStr(Arr[i]);
     end;
end;

end.


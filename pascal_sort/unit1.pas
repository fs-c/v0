unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonGenerate: TButton;
    ButtonBubbleSort: TButton;
    StringGrid1: TStringGrid;
    procedure ButtonGenerateClick(Sender: TObject);
    procedure ButtonBubbleSortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

    procedure WriteToTable(Arr: array of Integer);
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

procedure TForm1.ButtonGenerateClick(Sender: TObject);
var
  i: Integer;
begin
     randomize();

     for i := 0 to length(Numbers) - 1 do begin
         Numbers[i] := random(100000);
     end;

     WriteToTable(Numbers);
end;

procedure TForm1.ButtonBubbleSortClick(Sender: TObject);
var
  i, j, t: Integer;
begin
     for i := 0 to length(Numbers) - 1 do begin
         t := 0;
         for j := 0 to length(Numbers) - 1 do begin
             if Numbers[j] > Numbers[j + 1] then begin
                t := Numbers[j];
                Numbers[j] := Numbers[j + 1];
                Numbers[j + 1] := t;
             end;
         end;

         if t = 0 then
            break;
     end;

     WriteToTable(Numbers);
end;

procedure TForm1.WriteToTable(Arr: array of Integer);
var
  i: Integer;
begin
     for i := 0 to length(Arr) - 1 do begin
         StringGrid1.cells[i mod 15, i div 15] := IntToStr(Arr[i]);
     end;
end;

end.


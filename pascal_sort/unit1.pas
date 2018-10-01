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

    procedure WriteToGrid(Arr: array of Integer; Grid: TStringGrid);
  private

  public

  end;

var
  Form1: TForm1;
  Numbers: array [0..20000] of Integer;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonGenerateClick(Sender: TObject);
var
  i: Integer;
begin
     Randomize;

     for i := 0 to Length(Numbers) - 1 do begin
         { Fill with Integers from [0, 100000). }
         Numbers[i] := random(100000);
     end;

     WriteToGrid(Numbers, StringGrid1);
end;

procedure TForm1.ButtonBubbleSortClick(Sender: TObject);
var
  outer_i, inner_i, tmp: Integer;
begin
     for outer_i := 0 to Length(Numbers) - 1 do begin
         { Set to zero to be able to detect bailout condition. }
         tmp := 0;

         for inner_i := 0 to Length(Numbers) - 1 do
             if Numbers[inner_i] > Numbers[inner_i + 1] then begin
                { Swap values. }
                tmp := Numbers[inner_i];
                Numbers[inner_i] := Numbers[inner_i + 1];
                Numbers[inner_i + 1] := tmp;
             end;

         { If we never swapped values in this run, break out. }
         if tmp = 0 then
            break;
     end;

     WriteToGrid(Numbers, StringGrid1);
end;

{ Writes an array of Integers of variable size to a given StringGrid. Fails if
  the array does not fit into the grid. Moves horizontal first, from left to
  right and top to bottom. }
procedure TForm1.WriteToGrid(Arr: array of Integer; Grid: TStringGrid);
var
  i: Integer;
begin
     if Grid.ColCount * Grid.RowCount < Length(Arr) then
        Exit;

     for i := 0 to length(Arr) - 1 do begin
         Grid.cells[i mod 15, i div 15] := IntToStr(Arr[i]);
     end;
end;

end.


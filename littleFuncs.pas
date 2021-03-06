unit littleFuncs;

interface

  procedure SwapIntegers(var i1,i2: Integer);
  procedure SwapFloats(var f1,f2: Real);
  procedure SwapVariants(var v1,v2: Variant);

  function ByteSwap(const a: integer): integer;
  function ByteSwap16(inp:word): word;

  function DataToHex(aData: Pointer; aSize: Cardinal):String;


implementation

uses sysUtils, windows;

procedure SwapIntegers(var i1,i2: Integer);
var t: Integer;
begin
  t:=i1; i1:=i2; i2:=t;
end;

procedure SwapFloats(var f1,f2: Real);
var t: Real;
begin
  t:=f1; f1:=f2; f2:=t;
end;

procedure SwapVariants(var v1,v2: Variant);
var t: TVarData;
begin
//�������� ����� - ���� � 8 ������ �������� ��������� �� ������ ������� ������,
//�� ��� ������ �� ����������, ����� ����� ��� ������.
//���� �� �� �������� t:=v1; v1:=v2; v2:=t, ��������� �� ����������� �����������,
//�������� � ��. - ������� ���������.
  t:=TVarData(v1); TVarData(v1):=TVarData(v2); TVarData(v2):=t;
end;

{Invert bytes using assembly}
function ByteSwap(const a: integer): integer;
asm
  bswap eax
end;

function ByteSwap16(inp:word): word;
asm
  bswap eax
  shr   eax, 16
end;

type T2Char = array [0..1] of Char;
      P2Char = ^T2Char;

var DataToHexArray: Array [0..255] of T2Char;


function DataToHex(aData: Pointer; aSize: Cardinal): string;
var
  i: Integer;
  DataArray: PByteArray absolute aData;
begin
  SetLength(Result,aSize*2);
  for i := 0 to aSize-1 do
    CopyMemory(@Result[1+i shl 1],@DataToHexArray[DataArray^[i]],SizeOf(T2Char));
end;

procedure PrepareDataToHexArray;
var i: Integer;
    str: string;
begin
  for i := 0 to 255 do begin
    str:=IntToHex(i,2);
    DataToHexArray[i]:=P2Char(@str[1])^;
  end;
end;

initialization
  PrepareDataToHexArray;
end.

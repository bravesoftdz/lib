unit simple_parser_lib;

interface

uses SysUtils,ConvUtils;

type

TSimpleParser=class
  private
    _str: string;
    _pos: Integer;
    fBackupPos: Integer;
    procedure skip_spaces;
//    procedure find_next_space;
    function isSpace(ch: char): boolean;
    function isDelimiter(ch: char): boolean;
    function isFirstIdentSymbol(ch: char): boolean;
    function isIdentSymbol(ch: char): boolean;
    function isNumberSymbol(ch: char): boolean;
    function isPhysIdentSymbol(ch: char): boolean;
  public
    spaces: string;
    delimiter: string;
    constructor Create; overload;
    constructor Create(str: String); overload;
    procedure AssignString(str: String);
    procedure PutBack;  //что мы в прошлый раз вз€ли - "отдаем"
    function eof: boolean;
    function getChar: char;
    function NextChar: char;
    function getFloat: Real;
    function getInt: Integer;
    function getInt64: Int64;
    function getString: string;   //считать остаток строки
    function getIdent: string; //считать символы, пока они укладываютс€ в диап. A..Z, a..z, 0..9, _, ј..я, а..€
    function getPhysUnitIdent: string;
    function getHex: Integer;
    function getBinary: LongWord;
    function getPhysUnit: TConvType;
    function getVariantNum: Variant;
  end;

EParserError=class(Exception);

implementation
uses strUtils,Variants,VarCmplx,phys_units_lib;

constructor TSimpleParser.Create;
begin
  inherited Create;
  spaces:=' '+#9+#10+#13;
  delimiter:=';';
  _pos:=1;
  fbackupPos:=-1; //нечего возвращать
end;

constructor TSimpleParser.Create(str: String);
begin
  Create;
  _str:=str;
end;

procedure TSimpleParser.AssignString(str: String);
begin
  _str:=str;
  _pos:=1;
end;

function TSimpleParser.eof: Boolean;
var buPos: Integer;
begin
  buPos:=_pos;
  skip_spaces;
  result:=(_pos>Length(_str));
  _pos:=buPos;
end;

function TSimpleParser.isSpace(ch: Char): Boolean;
var i: Integer;
begin
  for i:=1 to Length(spaces) do
    if ch=spaces[i] then begin
      result:=true;
      exit;
    end;
  result:=false;
//  result:= (ch=' ') or (ch=#9) or (ch=#10) or (ch=#13);
end;

function TSimpleParser.isDelimiter(ch: Char): Boolean;
var i: Integer;
begin
  for i:=1 to Length(delimiter) do
    if ch=delimiter[i] then begin
      result:=true;
      exit;
    end;
  result:=false;
end;

function TSimpleParser.isFirstIdentSymbol(ch: Char): Boolean;
begin
  result:=((ch>='A') and (ch<='Z')) or ((ch>='a') and (ch<='z')) or
    ((ch>='ј') and (ch<='я')) or ((ch>='а') and (ch<='€')) or (ch='_');
end;

function TSimpleParser.isPhysIdentSymbol(ch: Char): Boolean;
begin
(*
  result:=((ch>='A') and (ch<='Z')) or ((ch>='a') and (ch<='z')) or
    ((ch>='ј') and (ch<='я')) or ((ch>='а') and (ch<='€')) or (ch='_') or
    (ch='$') or (ch='.') or (ch='%') or (ch='''') or (ch='"');
    *)
//проще сказать, чего нельз€: чисел, знаков +-*/, скобок
  result:=(ch<>'+') and (ch<>'-') and (ch<>'*') and (ch<>'/') and (ch<>'(') and
    (ch<>')') and not isSpace(ch) and (ch<>'[') and (ch<>']') and (ch<>'^');
end;

function TSimpleParser.isNumberSymbol(ch: Char): Boolean;
begin
  result:=(ch>='0') and (ch<='9');
end;

function TSimpleParser.isIdentSymbol(ch: Char): Boolean;
begin
  result:=IsFirstIdentSymbol(ch) or isNumberSymbol(ch);
end;



procedure TSimpleParser.PutBack;
begin
  if fBackupPos=-1 then raise EParserError.CreateFmt('Putback at %s: nothing to put back',[GetString]);
  _pos:=fBackupPos;
  fBackupPos:=-1;
end;

procedure TSimpleParser.skip_spaces;
begin
  while (_pos<=Length(_str)) and (isSpace(_str[_pos]) or isDelimiter(_str[_pos])) do inc(_pos);
end;
(*
procedure TSimpleParser.find_next_space;
begin
  while (_pos<=Length(_str)) and (not isSpace(_str[_pos])) do inc(_pos);
end;
*)
function TSimpleParser.NextChar: char;
begin
  skip_spaces;
  if _pos<=Length(_str) then result:=_str[_pos] else Raise EParserError.Create('TParser.NextChar: unexpected end of string');
end;

function TSimpleParser.getChar: char;
begin
  skip_spaces;
  fBackupPos:=_pos;
  inc(_pos);
  if fBackupPos<=Length(_str) then result:=_str[fBackupPos] else Raise EParserError.Create('TParser.getChar: unexpected end of string');
end;

function TSimpleParser.getFloat: Real;
var ch: char;
    s: string;
    separator: char;
begin
  separator:=DecimalSeparator;
  skip_spaces;
  fBackupPos:=_pos;
  while (_pos<=Length(_str)) and not isDelimiter(_str[_pos]) do begin
    ch:=_str[_pos];
    if ((ch>'9') or (ch<'0')) and (ch<>'+') and (ch<>'-') and (ch<>'E') and (ch<>'e') and (ch<>'.') and (ch<>',') and (not isSpace(ch)) then break;
    if (ch=',') or (ch='.') then ch:=separator;
    if not isSpace(ch) then s:=s+ch;
    inc(_pos);
  end;

  result:=StrToFloat(s);
end;

function TSimpleParser.getInt: Integer;
var ch: char;
    s: string;
begin
  skip_spaces;
  fBackupPos:=_pos;
  while (_pos<=Length(_str)) and not isDelimiter(_str[_pos]) do begin
    ch:=_str[_pos];
    if ((ch>'9') or (ch<'0')) and (ch<>'-') and (not isSpace(ch)) then break;
    if not isSpace(ch) then s:=s+ch;
    inc(_pos);
  end;
  if s='' then result:=0 else result:=StrToInt(s);
end;

function TSimpleParser.getInt64: Int64;
var ch: char;
    s: string;
begin
  skip_spaces;
  fBackupPos:=_pos;
  while (_pos<=Length(_str)) and not isDelimiter(_str[_pos]) do begin
    ch:=_str[_pos];
    if ((ch>'9') or (ch<'0')) and (ch<>'-') and (not isSpace(ch)) then break;
    if not isSpace(ch) then s:=s+ch;
    inc(_pos);
  end;
  if s='' then result:=0 else result:=StrToInt64(s);
end;

function TSimpleParser.getString: string;
begin
//  Result:=RightStr(_str,Length(_str)-_pos+1);
  skip_spaces;
  fBackupPos:=_pos;
  while (_pos<=Length(_str)) and (not isDelimiter(_str[_pos])) do inc(_pos);
  Result:=MidStr(_str,fBackupPos,_pos-fBackupPos);
end;

function TSimpleParser.getHex: Integer;
var x,Int0: Integer;
    ch: Integer;
begin
  int0:=Integer('0');
  x:=0;
  skip_spaces;
  fBackupPos:=_pos;
  while (_pos<=Length(_str)) and (_str[_pos]<='F') and (_str[_pos]>='0') do begin
    ch:=Integer(_str[_pos])-Int0;
    if ch>9 then ch:=ch-7;
    x:=(x shl 4) or ch;
    inc(_pos);
  end;
  Result:=x;
end;

function TSimpleParser.getBinary: Cardinal;
var Int0: Integer;
    ch: Cardinal;
    x: Cardinal;
begin
  int0:=Integer('0');
  x:=0;
  skip_spaces;
  fBackupPos:=_pos;
  while(_pos<=Length(_str)) and (_str[_pos]<='1') and (_str[_pos]>='0') do begin
    ch:=Integer(_str[_pos])-Int0;
    x:=(x shl 1) or ch;
    inc(_pos);
  end;
  Result:=x;
end;

function TSimpleParser.getIdent: string;
begin
  skip_spaces;
  fBackupPos:=_pos;
  if (_pos<=Length(_str)) and IsFirstIdentSymbol(_str[_pos]) then begin
    inc(_pos);
    while (_pos<=Length(_str)) and IsIdentSymbol(_str[_pos]) do inc(_pos);
  end;
  Result:=MidStr(_str,fBackupPos,_pos-fBackupPos);
end;

function TSimpleParser.getPhysUnitIdent: string;
begin
  skip_spaces;
  fBackupPos:=_pos;
  if (_pos<=Length(_str)) and IsPhysIdentSymbol(_str[_pos]) then begin
    inc(_pos);
    while (_pos<=Length(_str)) and IsPhysIdentSymbol(_str[_pos]) do inc(_pos);
  end;
  Result:=MidStr(_str,fBackupPos,_pos-fBackupPos);
end;

function TSimpleParser.getPhysUnit: TConvType;
var id: string;
    InitPos,tempPos: Integer;
    CType: TConvType;
    ch: char;
begin
  //хитрость в том, чтобы найти окончание.
  //скажем, 1 км*2 - здесь ед. изм "км"
  //но в 1 Ќ*м - "Ќ*м".
  skip_spaces;
  InitPos:=_pos;  //fBackupPos будет мен€тьс€ внутри цикла
  TempPos:=_pos;
  while not eof do begin
    id:=GetPhysUnitIdent;
    //хот€ у нас есть безразмерна€ величина, принимать
    //пустое место за нее не имеем права!
    if (id='') or not PrefixDescrToConvType(id,CType) then begin
      _pos:=TempPos;
      break;
    end
    else begin
      if eof then break;
      TempPos:=_pos;
      ch:=GetChar;
      if ch='^' then begin
        GetFloat;
        TempPos:=_pos;
        if eof then break;
        ch:=GetChar;
      end;
      if eof or ((ch<>'*') and (ch<>'/')) then begin
        _pos:=TempPos;
        break;
      end;
    end;
  end;

  fBackupPos:=InitPos;
  if _pos<>InitPos then
    Result:=StrToConvType(MidStr(_str,InitPos,_pos-InitPos))
  else
    Result:=CIllegalConvType;
end;

function TSimpleParser.getVariantNum: Variant;
var Ch: char;
  state: Integer;
  deg,min: Integer;
  sec: Real;

  function TryExponent: Boolean;
  begin
    Result:=(ch='e') or (ch='E');
    if Result then begin
      inc(_pos);
      if _pos>Length(_str) then begin
        putBack;
        Raise EParserError.CreateFmt('getVariantNum: unexpected end of string "%s"',[GetString]);
      end;
      ch:=_str[_pos];
      if (ch='-') or (ch='+') then state:=4
      else if IsNumberSymbol(ch) then state:=5
      else begin
        putBack;
        Raise EParserError.CreateFmt('getVariantNum: unknown symbol after exponent, "%s"',[GetString]);
      end;
    end;
  end;

  procedure TryImaginary;
  begin
    if (ch='i') or (ch='I') or (ch='j') or (ch='J') then
      inc(_pos);
  end;

begin
  Result:=Unassigned;
  //в самом числе не может содержатьс€ никаких пробелов!
  skip_spaces;
  fBackupPos:=_pos;
  state:=0;
  while (_pos<=Length(_str)) do begin
    Ch:=_str[_pos];
    case state of
      0: begin
          if IsNumberSymbol(ch) then state:=1 else begin
            putBack;
            Raise EParserError.CreateFmt('getVariantNum: digit expected in "%s"',[GetString]);
          end;
        end;
      1: begin
          if not IsNumberSymbol(ch) then
            if (ch='.') or (ch=',') then begin
              state:=2;
              _str[_pos]:=DecimalSeparator;
            end
            else if (ch='d') or (ch='D') or (ch='∞') then begin
              deg:=StrToInt(MidStr(_str,fBackupPos,_pos-fBackupPos));
              getChar;
              min:=getInt;
              if getChar<>'''' then Raise EParserError.Create(''' symbol (minutes) expected');
              sec:=getFloat;
              ch:=getChar;
              if (ch<>'"') and (getChar<>'''') then Raise EParserError.Create('" symbol (seconds) expected');
              Result:=VarWithUnitCreateFromVariant(deg+min/60+sec/3600,auDMS);
              Exit;
            end
            else if not TryExponent then begin
              TryImaginary;
              break;
            end;
        end;
      2: if IsNumberSymbol(ch) then state:=3 else begin
          putBack;
          Raise EParserError.CreateFmt('getVariantNum: digit expected after decimal separator in "%s"',[GetString]);
        end;
      3:  if not IsNumberSymbol(ch) then
            if not TryExponent then begin
              TryImaginary;
              break;
            end;
      4: if IsNumberSymbol(ch) then state:=5 else begin
          putBack;
          Raise EParserError.CreateFmt('getVariantNum: digit expected after exponent in "%s"',[GetString]);
        end;
      5: if not IsNumberSymbol(ch) then begin
          TryImaginary;
          break;
          end;
      end;
    inc(_pos);
  end;
  if state=0 then Raise EParserError.Create('getVariantNum: empty expression');
  if state=2 then begin
    PutBack;
    Raise EParserError.CreateFmt('getVariantNum: digit expected after decimal separator in "%s"',[GetString]);
  end;
  if state=4 then begin
    PutBack;
    Raise EParserError.CreateFmt('getVariantNum: digit expected after exponent sign in "%s"',[GetString]);
  end;

  if (UpperCase(_str[_pos-1])='I') or (UpperCase(_str[_pos-1])='J') then
    Result:=VarComplexCreate(0,StrToFloat(MidStr(_str,fBackupPos,_pos-fBackupPos-1)))
  else
    Result:=StrToFloat(MidStr(_str,fBackupPos,_pos-fBackupPos));
end;

end.

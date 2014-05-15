unit PersistentStringGrid;

interface

uses grids,classes(*,controls*);

type

TPersistentStringGrid=class(TStringGrid)
  private
    FDelimiter: char;
    procedure WriteData(writer: TWriter);
    procedure ReadData(reader: TReader);
    procedure SetText(value: string);
    function GetText: string;
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(owner: TComponent); override;
    procedure Assign(source: TPersistent); override;
    procedure AssignTo(dest: TPersistent); override;
    procedure SaveToTextFile(FileName: string);
    procedure LoadFromTextFile(FileName: string);
    procedure CopyClipbrd;
    procedure PasteFromClipbrd;
    procedure ExportToExcel;
    procedure AutoSize;
    property AsText: string read GetText write SetText;
  published
    property Delimiter: char read FDelimiter write FDelimiter;
end;

implementation
uses sysUtils,simple_parser_lib,clipbrd,strUtils,ComObj,Variants;

constructor TPersistentStringGrid.Create(owner: TComponent);
begin
  inherited Create(owner);
  Delimiter:=#9;
end;

procedure TPersistentStringGrid.Assign(source: TPersistent);
var s: TStringGrid;
    i,j: Integer;
begin
  if source is TStringGrid then begin
    s:=TStringGrid(source);
    //будем копировать содержимое, но не сам внеш. вид
    ColCount:=s.ColCount;
    RowCount:=s.RowCount;
    for j:=0 to RowCount-1 do
      for i:=0 to ColCount-1 do
        Cells[i,j]:=s.Cells[i,j];
  end
  else inherited Assign(source);
end;

procedure TPersistentStringGrid.AssignTo(dest: TPersistent);
var d: TStringGrid;
    i,j: Integer;
begin
  if dest is TStringGrid then begin
    d:=TStringGrid(dest);
    //будем копировать содержимое, но не сам внеш. вид
    d.ColCount:=ColCount;
    d.RowCount:=RowCount;
    for j:=0 to RowCount-1 do
      for i:=0 to ColCount-1 do
        d.Cells[i,j]:=Cells[i,j];
  end
  else inherited AssignTo(dest);
end;
procedure TPersistentStringGrid.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('data',ReadData,WriteData,true);
end;

procedure TPersistentStringGrid.WriteData(writer: TWriter);
var j,i: Integer;
begin
  Writer.WriteListBegin;
  for j:=0 to RowCount-1 do
    for i:=0 to ColCount-1 do
      if Cells[i,j]<>'' then begin
        Writer.writeString('('+IntToStr(i)+';'+IntToStr(j)+'):'+Cells[i,j]);
      end;
  Writer.WriteListEnd;
end;

procedure TPersistentStringGrid.ReadData(reader: TReader);
var p: TSimpleParser;
    s: string;
    i,j: Integer;
begin
  p:=TSimpleParser.Create;
  p.delimiter:='';
  reader.ReadListBegin;
  while not reader.EndOfList do begin
    s:=reader.ReadString;
    p.AssignString(s);
    if p.getChar<>'(' then Raise Exception.Create('TPersistentStringGrid.ReadData: no opening bracket');
    i:=p.getInt;
    if p.getChar<>';' then Raise Exception.Create('TPersistentStringGrid.ReadData: semicolon missing');
    j:=p.getInt;
    if p.getChar<>')' then Raise Exception.Create('TPersistentStringGrid.ReadData: no closing bracket');
    if p.getChar<>':' then Raise Exception.Create('TPersistentStringGrid.ReadData: no colon');
    Cells[i,j]:=p.getString;
  end;
  reader.ReadListEnd;
  p.Free;
end;

procedure TPersistentStringGrid.SaveToTextFile(FileName: string);
var i,j: Integer;
    s: string;
    F: textFile;
begin
  try
    AssignFile(F,FileName);
    Rewrite(F);
    for j:=0 to RowCount-1 do begin
      s:='';
      for i:=0 to ColCount-2 do
        s:=s+Cells[i,j]+Delimiter;
      s:=s+Cells[ColCount-1,j];
      WriteLn(F,s);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TPersistentStringGrid.LoadFromTextFile(FileName: string);
var i,j,x,y: Integer;
    s: string;
    F: textfile;
    backupfc,backupfr: Integer;
begin
  try
    AssignFile(F,FileName);
    Reset(F);
    backupfc:=fixedCols;
    backupfr:=FixedRows;
    RowCount:=0;
    ColCount:=0;
    y:=0;
    while not eof(F) do begin
      ReadLn(F,s);
      x:=0;
      j:=1;
      for i:=1 to Length(s) do begin
        if s[i]=Delimiter then begin
          if x+1>ColCount then ColCount:=x+1;
          if y+1>RowCount then RowCount:=y+1;
          Cells[x,y]:=MidStr(s,j,i-j);
          j:=i+1;
          inc(x);
        end;
      end;
      if j<Length(s) then begin
        if x+1>ColCount then ColCount:=x+1;
        Cells[x,y]:=MidStr(s,j,Length(s)-j+1);
      end;
      inc(y);
    end;
    FixedCols:=backupfc;
    fixedrows:=backupfr;
  finally
    CloseFile(F);
  end;
end;

function TPersistentStringGrid.GetText: string;
var i,j: Integer;
    s: string;
begin
  for j:=0 to RowCount-1 do begin
    for i:=0 to ColCount-2 do
      s:=s+Cells[i,j]+Delimiter;
    s:=s+Cells[ColCount-1,j]+#13+#10;
  end;
  Result:=s;
end;

procedure TPersistentStringGrid.SetText(value: string);
var i,j,x,y: Integer;
    backupfc,backupfr: Integer;
begin
  backupfc:=fixedCols;
  backupfr:=FixedRows;
  RowCount:=0;
  ColCount:=0;
  j:=1;
  y:=0;
  x:=0;
  for i:=1 to Length(value) do begin
    if value[i]=Delimiter then begin
      if x+1>ColCount then ColCount:=x+1;
      if y+1>RowCount then RowCount:=y+1;
      Cells[x,y]:=MidStr(value,j,i-j);
      j:=i+1;
      inc(x);
    end;
    if value[i]=#13 then begin
      //завершим сначала прошлую строку
      if x+1>ColCount then ColCount:=x+1;
      Cells[x,y]:=MidStr(value,j,i-j);
      //перевод строки
      inc(y);
      x:=0;
      j:=i+2;
    end;
  end;
  FixedCols:=backupfc;
  fixedrows:=backupfr;
end;

procedure TPersistentStringGrid.CopyClipbrd;
begin
  Clipboard.AsText:=AsText;
end;

procedure TPersistentStringGrid.PasteFromClipbrd;
begin
  AsText:=Clipboard.AsText;
end;

procedure TPersistentStringGrid.ExportToExcel;
var ExcelApp: Variant; //само приложение excel
    ExcelDoc: Variant; //новый документ
    ExcelSht: Variant; //новый лист
    i,j: Integer;
    cellname: string;
    val: Extended;
begin
  try
    ExcelApp:=CreateOleObject('Excel.Application');
    ExcelApp.visible:=false;
    ExcelDoc:=ExcelApp.Workbooks.Add;
    ExcelSht:=ExcelDoc.Worksheets.Add;
    for j:=0 to RowCount-1 do
      for i:=0 to ColCount-1 do begin
        cellname:=Chr(Integer('A')+i)+IntToStr(j+1);
//        ExcelSht.Range[cellname,cellname]:=Cells[i,j];
//очень странно он обращается с числами с плав. точкой - убивает в них запятую
          if Cells[i,j]<>'' then begin
            if TryStrToFloat(Cells[i,j],val) then
              ExcelSht.Cells.Item[j+1,i+1]:=val
            else
              ExcelSht.Cells.Item[j+1,i+1]:=Cells[i,j];
          end;

      end;
  finally
    if not VarIsEmpty(ExcelApp) then ExcelApp.visible:=true;
    ExcelApp:=UnAssigned;
  end;

end;

procedure TPersistentStringGrid.AutoSize;
var i,j,s,x: Integer;
    c: TStrings;
begin
  for i:=0 to ColCount-1 do begin
    c:=Cols[i];
    s:=0;
    for j:=0 to RowCount-1 do begin
      x:=Canvas.TextWidth(c[j])+5;
      if x>s then s:=x;
    end;
    ColWidths[i]:=s;
  end;
end;


end.

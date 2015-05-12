unit set_english_locale_if_not_sure;

interface

uses windows,classes,streaming_class_lib;

//пусть именно этот модуль хранит переменную - текущий выставленный язык
//и TLocalizedName на нее опирается

type
  TLangId = class(TPersistent)
    private
      fStrings: TStringList;
      procedure ReadData(reader: TReader);
      procedure WriteData(writer: TWriter);
    protected
      procedure DefineProperties(filer: TFiler); override;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(name: string; lang,sublang: Integer);
      function NameToID(name:string): Integer;
      function IDToName(id: integer): string;
  end;

  TLocalePreferences=class(TStreamingClass)
    private
      fLangId: TLangId;
    public
      constructor Create(Owner: TComponent); override;
      destructor Destroy; override;
    published
      property LangID: TLangId read fLangId write fLangId;
  end;

  TLocalizedName = class (TPersistent)   //опробуем композицию вместо наследования
  //можно указать это имя на любых языках и он это поймет, но возвращать будет
  //язык, поставленный в настройках
  //плюс, сохранение в файле и считывание из него
    private
      fStrings: TStringList;
      procedure ReadData(reader: TReader);
    protected
      procedure DefineProperties(filer: TFiler); override;
    public
      constructor Create;
      destructor Destroy; override;
  end;

procedure SetEnglishLocaleIfNotSure;
function GetDefaultLanguageInEnglish: string;



const US_ENGLISH = (SUBLANG_ENGLISH_US shl 10) or LANG_ENGLISH;
      UK_ENGLISH = (SUBLANG_ENGLISH_UK shl 10) or LANG_ENGLISH;

implementation

uses sysutils,reinit,simple_parser_lib;

(*
    General functions
                        *)
function GetDefaultLanguageInEnglish: string;
var loc_str: PChar;
    size: Integer;
begin
  Size:=GetLocaleInfo(LANG_USER_DEFAULT,LOCALE_SENGLANGUAGE,nil,0);
  loc_Str:=AllocMem(Size);
  GetLocaleInfo(LANG_USER_DEFAULT,LOCALE_SENGLANGUAGE,loc_str,size);
  Result:=loc_str;
  FreeMem(loc_str);
end;

procedure SetEnglishLocaleIfNotSure;
begin
  if (Uppercase(GetDefaultLanguageInEnglish)<>'RUSSIAN') and ((LoadNewResourceModule(US_ENGLISH)<>0) or
    (LoadNewResourceModule(UK_ENGLISH)<>0)) then
    ReinitializeForms;
end;

(*
    TLangID
                *)
constructor TLangID.Create;
begin
  inherited Create;
  fStrings:=TStringList.Create;
  fStrings.Sorted:=true;
  fStrings.Duplicates:=dupError;
end;

destructor TLangID.Destroy;
begin
  fStrings.Free;
  inherited Destroy;
end;

procedure TLangID.DefineProperties(filer: TFiler);
begin
  filer.DefineProperty('data',ReadData,WriteData,fStrings.Count>0);
end;

procedure TLangId.WriteData(writer: TWriter);
var i: Integer;
begin
  writer.WriteListBegin;
  for i:=0 to fStrings.Count-1 do
    writer.WriteString(Format('%s;%x;%x',[fStrings[i],
      Integer(fStrings.Objects[i]) and $03FF,Integer(fStrings.Objects[i]) shr 10]));
  writer.WriteListEnd;
end;

procedure TLangId.ReadData(reader: TReader);
var p: TSimpleParser;
    nm: string;
    lang,sublang: Integer;
begin
  reader.ReadListBegin;
  p:=TSimpleParser.Create;
  while not reader.EndOfList do begin
    p.AssignString(reader.ReadString);
    nm:=p.getString;
    lang:=p.getHex;
    sublang:=p.getHex;
    Add(nm,lang,sublang);
  end;
  p.Free;
  reader.ReadListEnd;
end;

procedure TLangId.Add(name: string; lang,sublang: Integer);
begin
  fstrings.AddObject(name,TObject(lang or (sublang shl 10)));
end;

function TLangID.NameToID(name: string): Integer;
var i: Integer;
begin
  i:=fstrings.IndexOf(name);
  if i>=0 then
    Result:=Integer(fstrings.Objects[i])
  else
    Raise Exception.CreateFmt('Language %s not found among langID',[name]);
end;

function TLangID.IDToName(id: Integer): string;
var i: Integer;
begin
  i:=fstrings.IndexOfObject(TObject(id));
  if i>=0 then
    Result:=fstrings[i]
  else
    Raise Exception.CreateFmt('Language with ID %x not found among LangID',[id]);
end;

(*
    TLocalePreferences
                          *)
constructor TLocalePreferences.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  LangId:=TLangId.Create;
end;

destructor TLocalePreferences.Destroy;
begin
  LangId.Free;
  inherited Destroy;
end;


(*
    TLocalizedName
                    *)
constructor TLocalizedName.Create;
begin
  inherited Create;
  fStrings:=TStringList.Create;
end;

destructor TLocalizedName.Destroy;
begin
  fStrings.Free;
  inherited Destroy;
end;

procedure TLocalizedName.DefineProperties(filer: TFiler);
begin
  filer.DefineProperty('data',ReadData,nil,fStrings.Count>0);
end;

procedure TLocalizedName.ReadData(reader: TReader);
begin
  

end;

end.

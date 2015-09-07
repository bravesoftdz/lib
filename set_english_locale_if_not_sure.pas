unit set_english_locale_if_not_sure;

interface

uses windows,classes,streaming_class_lib,contnrs;

//пусть именно этот модуль хранит переменную - текущий выставленный язык
//и TLocalizedName на нее опирается

type
  TLangIdType = Integer;

  TListWithID = class(TList)
    public
      ID: Integer;
  end;

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

  TLangPrefMatrix = class(TComponent)
    private
      fList: TObjectList;
      procedure ReadData(reader: TReader);
      procedure WriteData(writer: TWriter);
    protected
      procedure DefineProperties(filer: TFiler); override;
    public
      constructor Create(Owner: TComponent); override;
      destructor Destroy; override;
      procedure Add(prefList: TListWithID);
      function GetPrefList(ID: Integer): TListWithId;
    end;

  TLocalePreferences=class(TStreamingClass)
    private
      fLangId: TLangId;
      fLangPrefMatrix: TLangPrefMatrix;
      fCurLang,fResLang: Integer;
      fAvailLanguageList: TList;
      procedure SetLanguage(value: string);
      procedure SetLanguageID(value: Integer);
      function GetLanguage: string;
      procedure SetResourceLang(value: string);
      function GetResourceLang: string;
    public
      constructor Create(Owner: TComponent); override;
      constructor CreateDefault;
      destructor Destroy; override;
      procedure Loaded; override;
      function GetAppropriateLang(list: TListWithID): Integer;
      function TryGetMatchingString(ID: Integer; strings: TStrings; out str: string): Boolean;
      function TrySetMatchingString(ID: Integer; strings: TStrings; val: string): Boolean;
      function GetLanguageList: TStringList;  //string:название языка, object: его ID
      function ChangeLanguageID(value: Integer): Boolean;
      function ChangeLanguage(value: string): Boolean;
      property languageID: Integer read fCurLang write SetLanguageID;
      property ResourceLangID: Integer read fResLang;
    published
      property LangID: TLangId read fLangId write fLangId;
      property LangPrefMatrix: TLangPrefMatrix read fLangPrefMatrix write fLangPrefMatrix;
      property ResourceLang: string read GetResourceLang write SetResourceLang;
      property Language: string read GetLanguage write SetLanguage;
  end;

  TLocalizedName = class (TComponent)   //опробуем композицию вместо наследования
  //можно указать это имя на любых языках и он это поймет, но возвращать будет
  //язык, поставленный в настройках
  //плюс, сохранение в файле и считывание из него
    private
      fStrings: TStringList;
      procedure ReadData(reader: TReader);
      procedure WriteData(writer: TWriter);
    protected
      procedure DefineProperties(filer: TFiler); override;
      procedure SetMatchingString(lang,value: string);
      function GetMatchingString(lang: string): string;
    public
      constructor Create(aOwner: TComponent); override;
      constructor CreateEmptyNeutral(aOwner: Tcomponent=nil);
      procedure MakeEmptyNeutral;
      destructor Destroy; override;
      procedure Assign(source: TPersistent); override;
      procedure Clear;
      function Caption: string;
      function TryCaption(out Caption: string): Boolean;
      function MatchingString(Lang: TObject): string;
      function TryMatchingString(Lang: TObject; out val: string): Boolean;
      function TryEnglish(out val: string): Boolean;
      function InEnglish: string;
      function Enabled: Boolean;
      function isNeutral(index: Integer): Boolean;
      procedure AddString(str, langName: string);
      procedure AddInCurrentLang(str: string);
      procedure LeftConcat(source: TLocalizedName; chr: string=''); overload;
      procedure LeftConcat(str: string); overload;
      procedure RightConcat(source: TLocalizedName; chr: string=''); overload;
      procedure RightConcat(str: string); overload;
      property strings: TStringList read fstrings;
      property str[Lang: string]: string read GetMatchingString write SetMatchingString;
//      function EqualsTo(value: string): Boolean;
  end;

procedure SetEnglishLocaleIfNotSure;
function GetDefaultLanguageInEnglish: string;

function LocaleCompareStr(s1,s2: string): Integer;
function LocaleCompareText(s1,s2: string): Integer;

var LocalePreferences: TLocalePreferences;

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

function GetDefaultLanguageID: Integer;
var loc_str: PChar;
    size: Integer;
begin
  Size:=GetLocaleInfo(LANG_USER_DEFAULT,LOCALE_ILANGUAGE,nil,0);
  loc_Str:=AllocMem(Size);
  GetLocaleInfo(LANG_USER_DEFAULT,LOCALE_ILANGUAGE,loc_str,size);
  Result:=StrToInt(loc_str);
  FreeMem(loc_str);
end;

function GetLanguageNameInCurLocale(Lang_ID: Integer): string;
var loc_str: PChar;
    size: Integer;
begin
  Size:=GetLocaleInfo(Lang_ID,LOCALE_SLANGUAGE,nil,0);
  loc_Str:=AllocMem(Size);
  GetLocaleInfo(Lang_ID,LOCALE_SLANGUAGE,loc_str,size);
  Result:=loc_str;
  FreeMem(loc_str);
end;


procedure SetEnglishLocaleIfNotSure;
//var buDir: string;
begin
//вроде как localePrefences уже загружен
  

//  buDir:=GetCurrentDir;

//  localePreferences:=TLocalePreferences.LoadFromFile(buDir+'\data\Lang\LanguageSettings.txt');
//  SetCurrentDir(buDir);
end;

(*
    TLangID
                *)
constructor TLangID.Create;
begin
  inherited Create;
  fStrings:=TStringList.Create;
  fstrings.CaseSensitive:=false;
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
    TLangPrefMatrix
                          *)
function LanguageListCompare (Item1, Item2: Pointer): Integer;
var i1: TListWithId absolute Item1;
    i2: TListWIthId absolute Item2;
begin
  if i1.id>i2.ID then Result:=1
  else if i1.Id<i2.ID then Result:=-1
  else Result:=0;
end;

constructor TLangPrefMatrix.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fList:=TObjectList.Create(true);
end;

destructor TLangPrefMatrix.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

procedure TLangPrefMatrix.DefineProperties(filer: TFiler);
begin
  filer.DefineProperty('data',ReadData,WriteData,fList.Count>0);
end;

procedure TLangPrefMatrix.WriteData(writer: TWriter);
var i,j: Integer;
  langID: TLangId;
begin
  langID:=(Owner as TLocalePreferences).LangID;
  writer.WriteListBegin;
  for i:=0 to fList.Count-1 do
    with TListWithID(fList[i]) do begin
      writer.WriteIdent(langID.IDToName(ID));
      writer.WriteListBegin;
      for j:=0 to Count-1 do
        writer.WriteIdent(langID.IDToName(Integer(Items[j])));
      writer.WriteListEnd;
    end;
  writer.WriteListEnd;
end;

procedure TLangPrefMatrix.ReadData(reader: TReader);
var langID: TLangId;
    lst: TListWithID;
begin
  langID:=(Owner as TLocalePreferences).LangID;
  reader.ReadListBegin;
  while not reader.EndOfList do begin
    lst:=TListWithID.Create;
    lst.ID:=langID.NameToID(reader.ReadIdent);
    reader.ReadListBegin;
    while not reader.EndOfList do
      lst.Add(Pointer(langID.NameToID(reader.ReadIdent)));
    Add(lst);
    reader.ReadListEnd;
  end;
  reader.ReadListEnd;
  fList.Sort(LanguageListCompare);
end;

procedure TLangPrefMatrix.Add(prefList: TListWithId);
begin
  fList.Add(prefList);
end;

function TLangPrefMatrix.GetPrefList(ID: Integer): TListWithId;
var i: Integer;
begin
  for i:=0 to flist.Count-1 do begin
    Result:=TListWithID(flist[i]);
    if Result.ID=ID then Exit
    else if Result.ID>ID then break;
  end;
  Result:=nil;
end;


(*
    TLocalePreferences
                          *)
constructor TLocalePreferences.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  LangId:=TLangId.Create;
  LangPrefMatrix:=TLangPrefMatrix.Create(self);
  LangPrefMatrix.SetSubComponent(true);
  fAvailLanguageList:=TList.Create;
end;

constructor TLocalePreferences.CreateDefault;
var lst: TListWithID;
begin
  Create(nil);
  //англ. язык необходим
  LangID.Add('English',9,1);
  lst:=TListWithID.Create;
  lst.ID:=1033;
  lst.Add(Pointer(0));  //neutral/any на первом месте
  lst.Add(Pointer(1033)); //след. - английский
  LangPrefMatrix.Add(lst);
end;

destructor TLocalePreferences.Destroy;
begin
  LangId.Free;
  fAvailLanguageList.Free;
  inherited Destroy;
end;

procedure TLocalePreferences.Loaded;
// var i: Integer;
begin
  inherited Loaded;
  fAvailLanguageList.Clear;
(*
  for i:=0 to LangId.fStrings.Count-1 do
    if (Cardinal(LangId.fStrings.Objects[i])<>0) and (LoadNewResourceModule(Cardinal(LangId.fStrings.Objects[i]))<>0) then
      fAvailLanguageList.Add(LangId.fStrings.Objects[i]);
*)
  if fCurLang=0 then
//    LanguageID:=GetDefaultLanguageID;
    Language:=GetDefaultLanguageInEnglish;
end;

procedure TLocalePreferences.SetLanguage(value: string);
begin
  SetLanguageID(LangID.NameToID(value));
end;

procedure TLocalePreferences.SetLanguageID(value: Integer);
begin
  ChangeLanguageID(value);
end;

function TLocalePreferences.GetResourceLang: string;
begin
  Result:=LangID.IDToName(fResLang);
end;

procedure TLocalePreferences.SetResourceLang(value: string);
begin
  fResLang:=LangID.NameToID(value);
end;

function TLocalePreferences.ChangeLanguage(value: string): Boolean;
begin
  Result:=ChangeLanguageID(LangID.NameToID(value));
end;

function TLocalePreferences.ChangeLanguageID(value: integer): Boolean;
var i: Integer;
    preflist: TListWithID;
begin
  Result:=false;
  if value<>fCurLang then begin
    preflist:=LangPrefMatrix.GetPrefList(value);
    if Assigned(preflist) then
      for i:=0 to preflist.Count-1 do
        if (Cardinal(preflist[i])<>0) and (LoadNewResourceModule(Cardinal(preflist[i]))<>0) then begin
          fCurLang:=value;
          ReinitializeForms;
          Result:=true;
          break;
        end;
  end;
end;

function TLocalePreferences.GetLanguage: string;
begin
  Result:=LangID.IDToName(fCurLang);
end;

function TLocalePreferences.GetAppropriateLang(list: TListWithId): Integer;
var preflist: TListWithID;
    i: Integer;
begin
//в list.ID лежит искомый язык, а в самом списке - варианты замены
  preflist:=LangPrefMatrix.GetPrefList(list.ID);
  if assigned(preflist) then begin
    for i:=0 to preflist.Count-1 do begin
      Result:=Integer(preflist[i]);
      if list.IndexOf(Pointer(Result))>=0 then Exit;
    end;
    //если дошли до этого места, значит, списки list и preflist не пересеклись
    //ну и хрен с ним - не будем гадать, лучше выругаемся
  end;
  raise Exception.CreateFmt('Didn''t find appropriate replacement for %s language',[LangId.IDToName(list.ID)]);
end;

function TLocalePreferences.TryGetMatchingString(ID: Integer;
  strings: TStrings; out str: string): Boolean;
var preflist: TListWithID;
    i,j: Integer;
begin
  preflist:=LangPrefMatrix.GetPrefList(ID);
  Result:=false;
  if assigned(preflist) then
    for i:=0 to preflist.Count-1 do begin
      j:=strings.IndexOfObject(preflist[i]);
      if j>=0 then begin
        Result:=true;
        str:=strings[j];
        Exit;
      end;
    end;
end;

function TLocalePreferences.TrySetMatchingString(ID: Integer; strings: TStrings;
  val: string): Boolean;
var preflist: TListWithID;
    i,j: Integer;
begin
  preflist:=LangPrefMatrix.GetPrefList(ID);
  Result:=false;
  if assigned(preflist) then
    for i:=0 to preflist.Count-1 do begin
      j:=strings.IndexOfObject(preflist[i]);
      if j>=0 then begin
        Result:=true;
        strings[j]:=val;
        Exit;
      end;
    end;
end;

function TLocalePreferences.GetLanguageList: TStringList;
var i: Integer;
begin
  Result:=TStringList.Create;
  for i:=0 to fAvailLanguageList.Count-1 do
    Result.AddObject(GetLanguageNameInCurLocale(Integer(fAvailLanguageList[i])),
      TObject(fAvailLanguageList[i]));
end;


(*
    TLocalizedName
                    *)
constructor TLocalizedName.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  SetSubComponent(true);
  fStrings:=TStringList.Create;
end;

constructor TLocalizedName.CreateEmptyNeutral(aOwner: TComponent=nil);
begin
  Create(aOwner);
  strings.AddObject('',nil);
end;

procedure TLocalizedName.MakeEmptyNeutral;
begin
  strings.Clear;
  strings.AddObject('',nil);
end;

destructor TLocalizedName.Destroy;
begin
  fStrings.Free;
  inherited Destroy;
end;

procedure TLocalizedName.DefineProperties(filer: TFiler);
begin
  filer.DefineProperty('data',ReadData,WriteData,fStrings.Count>0);
end;

procedure TLocalizedName.ReadData(reader: TReader);
var p: TSimpleParser;
    str: string;
begin
  reader.ReadListBegin;
  p:=TSimpleParser.Create;
  while not reader.EndOfList do begin
    p.AssignString(reader.ReadString);
    str:=p.getString;
    AddString(str,p.getString);
  end;
  p.Free;
  reader.ReadListEnd;
end;

procedure TLocalizedName.WriteData(writer: TWriter);
var i: Integer;
begin
  writer.WriteListBegin;
  for i:=0 to fStrings.Count-1 do
    writer.WriteString(Format('%s;%s',
      [fStrings[i],LocalePreferences.LangID.IDToName(Integer(fstrings.Objects[i]))]));
  writer.WriteListEnd;
end;

procedure TLocalizedName.AddString(str,langName: string);
begin
  fStrings.AddObject(str,TObject(LocalePreferences.LangID.NameToId(langName)));
end;

procedure TLocalizedName.AddInCurrentLang(str: string);
begin
  fstrings.AddObject(str,TObject(LocalePreferences.fCurLang));
end;

function TLocalizedName.TryMatchingString(Lang: TObject; out val: string): Boolean;
begin
  Result:=localePreferences.TryGetMatchingString(Integer(Lang),fstrings,val);
end;

function TLocalizedName.MatchingString(Lang: TObject): string;
//var i: Integer;
begin
//  i:=fstrings.IndexOfObject(Lang);
//  Result:=fstrings[i];
  if not TryMatchingString(Lang,Result) then
    if fstrings.Count=0 then Raise Exception.Create('empty locale string') else
    Raise Exception.CreateFMT('Couldn''t find string matching %s',[localePreferences.LangID.IDToName(Integer(Lang))]);
end;

function TLocalizedName.Caption: string;
begin
  Result:=MatchingString(TObject(LocalePreferences.languageID));
end;

function TLocalizedName.TryCaption(out Caption: string): Boolean;
begin
  Result:=TryMatchingString(TObject(LocalePreferences.languageID),Caption);
end;

function TLocalizedName.TryEnglish(out val: string): Boolean;
begin
  Result:=TryMatchingString(TObject(1033),val);
end;

function TLocalizedName.InEnglish: string;
begin
  Result:=MatchingString(Tobject(1033));
end;

function TLocalizedName.Enabled: Boolean;
begin
  Result:=(fstrings.Count>0);
end;

function TLocalizedName.isNeutral(index: Integer): Boolean;
begin
  Result:=Integer(fstrings.Objects[index])=0;
end;

procedure TLocalizedName.Assign(source: TPersistent);
var s: TLocalizedName absolute source;
begin
  if source is TLocalizedName then begin
    fstrings.Assign(s.fStrings);
  end
  else inherited Assign(source);
end;

procedure TLocalizedName.Clear;
begin
  strings.Clear;
end;

procedure TLocalizedName.LeftConcat(source: TLocalizedName; chr: string='');
var i,j: Integer;
    new_strings: TStringList;
begin //add source to the left of us, and we are main here
  new_strings:=TStringList.Create;
  for i:=0 to strings.Count-1 do
    if isNeutral(i) then
      for j:=0 to source.strings.Count-1 do
        new_strings.AddObject(source.strings[j]+chr+strings[i],source.strings.Objects[j])
    else begin
      j:=source.strings.IndexOfObject(strings.Objects[i]);
      if j>=0 then
        new_strings.AddObject(source.strings[j]+chr+strings[i],strings.Objects[i])
      else begin
        j:=source.strings.IndexOfObject(nil);
        if j>=0 then
          new_strings.AddObject(source.strings[j]+chr+strings[i],strings.Objects[i]);
      end;
    end;
  strings.Free;
  fstrings:=new_strings;
end;

procedure TLocalizedName.RightConcat(source: TLocalizedName; chr: string='');
var i,j: Integer;
    new_strings: TStringList;
begin //add source to the right of us, and we are main here
  new_strings:=TStringList.Create;
  for i:=0 to strings.Count-1 do
    if isNeutral(i) then
      for j:=0 to source.strings.Count-1 do
        new_strings.AddObject(strings[i]+chr+source.strings[j],source.strings.Objects[j])
    else begin
      j:=source.strings.IndexOfObject(strings.Objects[i]);
      if j>=0 then
        new_strings.AddObject(strings[i]+chr+source.strings[j],strings.Objects[i])
      else begin
        j:=source.strings.IndexOfObject(nil);
        if j>=0 then
          new_strings.AddObject(strings[i]+chr+source.strings[j],strings.Objects[i]);
      end;
    end;
//  if new_strings.Count=0 then
//    Assert(fstrings.Count>0,'RightConcat yielded empty locale string');
//    Raise Exception.CreateFmt('%s + %s yielded empty string',[strings.Text,source.strings.Text]);
  strings.Free;
  fstrings:=new_strings;    
end;

procedure TLocalizedName.LeftConcat(str: string);
var i: Integer;
begin
  for i:=0 to strings.Count-1 do
    strings[i]:=str+strings[i];
end;

procedure TLocalizedName.RightConcat(str: string);
var i: Integer;
begin
  for i:=0 to strings.Count-1 do
    strings[i]:=strings[i]+str;
end;

procedure TLocalizedName.SetMatchingString(lang,value: string);
begin
  if not localePreferences.TrySetMatchingString(localePreferences.LangID.NameToID(lang),strings,value) then
    Raise Exception.CreateFMT('SetMatchingString: lang %s not found',[lang]);
end;

function TLocalizedName.GetMatchingString(lang: string): string;
begin
  if not localePreferences.TryGetMatchingString(localePreferences.LangID.NameToID(lang),strings,Result) then
    Raise Exception.CreateFmt('GetMatchingString: lang %s not found',[lang]);
end;

(*
      General
                  *)
function LocaleCompareStr(s1,s2: string): Integer;
begin
  Result:=CompareString(localePreferences.ResourceLangID, 0, PChar(S1), Length(S1),
    PChar(S2), Length(S2)) - 2;
end;

function LocaleCompareText(s1,s2: string): Integer;
begin
  Result:=CompareString(localePreferences.ResourceLangID, NORM_IGNORECASE, PChar(S1), Length(S1),
    PChar(S2), Length(S2)) - 2;
end;


initialization
  if FileExists(GetCurrentDir+'\data\Lang\LanguageSettings.txt') then
    localePreferences:=TLocalePreferences.LoadFromFile(GetCurrentDir+'\data\Lang\LanguageSettings.txt')
  else
    localePreferences:=TLocalePreferences.CreateDefault;
finalization
  FreeAndNil(localePreferences);



end.

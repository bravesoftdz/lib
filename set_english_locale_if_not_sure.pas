unit set_english_locale_if_not_sure;

interface

uses windows;

procedure SetEnglishLocaleIfNotSure;

const US_ENGLISH = (SUBLANG_ENGLISH_US shl 10) or LANG_ENGLISH;
      UK_ENGLISH = (SUBLANG_ENGLISH_UK shl 10) or LANG_ENGLISH;

implementation

uses sysutils,reinit;

procedure SetEnglishLocaleIfNotSure;
var loc_str: PChar;
    size: Integer;
begin
  Size:=GetLocaleInfo(LANG_USER_DEFAULT,LOCALE_SENGLANGUAGE,nil,0);
  loc_Str:=AllocMem(Size);
  GetLocaleInfo(LANG_USER_DEFAULT,LOCALE_SENGLANGUAGE,loc_str,size);

  if (Uppercase(loc_str)<>'RUSSIAN') and ((LoadNewResourceModule(US_ENGLISH)<>0) or
    (LoadNewResourceModule(UK_ENGLISH)<>0)) then
    ReinitializeForms;
    
  FreeMem(loc_str);

end;

end.

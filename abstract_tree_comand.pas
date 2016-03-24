unit abstract_tree_comand;

interface

//чтобы историю изменений можно было хранить вместе со всем остальным
  TAbstractTreeCommand=class(TAbstractCommand)
    private
      fNext,fPrev,fBranch: TAbstractTreeCommand;
      fTurnLeft: Boolean;
    protected
      fActiveBranch: Boolean;
    public
      procedure Clear; override;

      function EqualsByAnyOtherName(what: TStreamingClass): boolean; override;
    published
//мы могли бы не сохранять Prev в файл, а восстанавливать его при загрузке
//cэкономит сколько-нибудь памяти, около 31 байт на команду
      property Prev: TAbstractTreeCommand read fPrev write fPrev;
      property Next: TAbstractTreeCommand read fNext write fNext;
      property Branch: TAbstractTreeCommand read fBranch write fBranch;
      property ActiveBranch: Boolean read fActiveBranch write fActiveBranch default false;
      property TurnLeft: Boolean read fTurnLeft write fTurnLeft default false;
    end;


implementation

 (*
        TAbstractTreeCommand
                                 *)
procedure TAbstractTreeCommand.Clear;
begin
  Next:=nil;
  Prev:=nil;
  Branch:=nil;
  ActiveBranch:=false;
  fImageIndex:=-1;
end;

function TAbstractTreeCommand.EqualsByAnyOtherName(what: TStreamingClass): boolean;
var t: TAbstractTreeCommand absolute what;
    buName: string;
    buNext,buPrev,buBranch: TAbstractTreeCommand;
    buActiveBranch,buTurnLeft: boolean;
    buHash: T4x4LongWordRecord;
    bin1,bin2: TMemoryStream;
    tmpName: string;
    ComponentWithSameName: TComponent;
begin
  if ClassType=what.ClassType then begin
    //нынче команды очень сложные пошли, завязанные на документ
    //выдернуть их из документа - не поймут, что происходит
    //придется действовать аккуратно...
    //сначала записываем наш родной компонент
    bin1:=TMemoryStream.Create;
    try
      bin1.WriteComponent(self);
      bin1.Seek(0,soFromBeginning);
//    self.saveFormat:=fCyr;
//    self.SaveToFile('wtf1.txt');
      //теперь можно поменять имя второй команде, если там не существует компонента с таким же
      //именем
      tmpName:=Name;
      if Assigned(t.Owner) and (Name<>t.Name) and Assigned(t.Owner.FindComponent(Name)) then begin
        //небольшая чехарда
        ComponentWithSameName:=t.Owner.FindComponent(Name);
        ComponentWithSameName.Name:='';
        //возможно, что это были мы...
      end
      else ComponentWithSameName:=nil;
      //можем безболезненно провести сравнение "на месте"
      buName:=t.Name;
      buNext:=t.next;
      buPrev:=t.prev;
      buBranch:=t.Branch;
      buActiveBranch:=t.ActiveBranch;
      buTurnLeft:=t.TurnLeft;
      t.Name:=tmpName;  //очень вероятно, что мы переименовали себя чтобы избежать конфликта
      t.Next:=Next;
      t.Prev:=Prev;
      t.Branch:=Branch;
      t.ActiveBranch:=ActiveBranch;
      t.TurnLeft:=TurnLeft;
      if t is THashedCommand then begin
        buHash:=HashedT.fHash;
        HashedT.fHash:=(self as THashedCommand).fHash;
      end;

      bin2:=TMemoryStream.Create;
      try
        bin2.WriteComponent(t);
        bin2.Seek(0,soFromBeginning);
        if bin1.Size<>bin2.Size then Result:=false
        else Result:=Comparemem(bin1.Memory,bin2.Memory,bin1.Size);
      finally
        bin2.free;
      end;
  //    if Result=false then begin
  //      what.saveFormat:=fCyr;
  //      what.SaveToFile('wtf2.txt');
  //    end;
    finally
      bin1.Free;
    end;

    t.Name:=buName;
    t.Next:=buNext;
    t.Prev:=buPrev;
    t.Branch:=buBranch;
    t.ActiveBranch:=buActiveBranch;
    t.TurnLeft:=buTurnLeft;

    if Assigned(ComponentWithSameName) then
      ComponentWithSameName.Name:=tmpName;
    if t is THashedCommand then
      HashedT.fHash:=buHash;
  end
  else Result:=false;
end;


end.

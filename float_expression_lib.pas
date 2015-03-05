unit float_expression_lib;

interface

uses classes,expression_lib;

type

TFloatExpression=class(TAbstractExpression)
  protected
    procedure MakeEvaluationTree; override;
    procedure PlusMinus(s: string; var treeNode: TEvaluationTreeNode);
    procedure MulDiv(s: string; var treeNode: TEvaluationTreeNode);
    procedure Pow(s: string; var treeNode: TEvaluationTreeNode);
    procedure BracketsAndFuncs(s: string; var treeNode: TEvaluationTreeNode);
  public
    function getValue: Real; override;
    function getVariantValue: Variant; override;
end;

TStandAloneFloatExpression = class (TFloatExpression)
  public
    constructor Create(Owner: TComponent); override;
end;


implementation

uses SysUtils, StrUtils;

procedure TFloatExpression.PlusMinus(s: string; var treeNode: TEvaluationTreeNode);
//treenode - это var-переменная, в которую мы должны положить адрес созданного узла
var i,last_plus: Integer;
    brCount: Integer;
    children: array of TEvaluationTreeNode;
    signCount: Integer;
    temp: TEvaluationTreeNode;
    isNeg: boolean;
begin
  try
  brCount:=0;
  last_plus:=1;
  isNeg:=false;
  signCount:=0;
  temp:=nil;
  for i:=1 to Length(s) do begin
    if brCount=0 then begin
      if ((s[i]='+') or (s[i]='-')) and ((i<=1) or (uppercase(s[i-1])<>'E')) then begin
        if i>1 then begin
          SetLength(children,Length(children)+1);
          MulDiv(MidStr(s,last_plus,i-last_plus),temp);
          if isNeg then begin
            children[Length(children)-1]:=TUnaryMinusNode.Create(nil);  //позже закрепим
            children[Length(children)-1].InsertComponent(temp);
          end
          else
          children[length(children)-1]:=temp;
        end;
        temp:=nil;
        last_plus:=i+1; //сразу за плюсом
        isNeg:=(s[i]='-');
        inc(signCount);
      end
    end;
    if s[i]='(' then inc(brCount)
    else if s[i]=')' then dec(brCount);
    if brCount<0 then Raise ESyntaxErr.CreateFMT(TooManyClosingBracketsStr,[s]);
  end;
  if signCount=0 then MulDiv(s,treeNode)
  else begin
    SetLength(children,Length(children)+1);
    MulDiv(RightStr(s,Length(s)-last_plus+1),temp);
    if isNeg then begin
      children[Length(children)-1]:=TUnaryMinusNode.Create(nil);  //позже закрепим
      children[Length(children)-1].InsertComponent(temp);
    end
    else
      children[length(children)-1]:=temp;
    temp:=nil;
    //вот, все "дети" в сборе!
    treeNode:=TAdditionNode.Create(nil);  //позже нас прикрепят, если надо
    for i:=0 to Length(children)-1 do begin
      treeNode.InsertComponent(children[i]);
      children[i]:=nil;
    end;
  end;


  finally
    for i:=0 to Length(children)-1 do
      children[i].Free;
    temp.Free;
  end;

end;

procedure TFloatExpression.MulDiv(s: string; var treeNode: TEvaluationTreeNode);
var i,last_plus: Integer;
    brCount: Integer;
    children: array of TEvaluationTreeNode;
    temp: TEvaluationTreeNode;
    isNeg: boolean;
begin
//  try
  brCount:=0;
  last_plus:=1;
  isNeg:=false;
  for i:=1 to Length(s) do begin
    if brCount=0 then begin
      if (s[i]='*') or (s[i]='/') then begin
        SetLength(children,Length(children)+1);
        Pow(MidStr(s,last_plus,i-last_plus),temp);
        if isNeg then begin
          children[Length(children)-1]:=TInverseNode.Create(nil);  //позже закрепим
          children[Length(children)-1].InsertComponent(temp);
        end
        else
          children[length(children)-1]:=temp;
        last_plus:=i+1; //сразу за плюсом
        isNeg:=(s[i]='/');
      end
    end;
    if s[i]='(' then inc(brCount)
    else if s[i]=')' then dec(brCount);
    if brCount<0 then Raise EsyntaxErr.CreateFMT(TooManyClosingBracketsStr,[s]);
  end;
  if Length(children)=0 then Pow(s,treeNode)
  else begin
    treeNode:=TMultiplicationNode.Create(nil);  //позже нас прикрепят, если надо
    for i:=0 to Length(children)-1 do begin
      treeNode.InsertComponent(children[i]);
      children[i]:=nil;
    end;
    Pow(RightStr(s,Length(s)-last_plus+1),temp);
    if isNeg then begin
      children[0]:=TInverseNode.Create(nil);
      children[0].InsertComponent(temp);
    end
    else
      children[0]:=temp;
    treeNode.InsertComponent(children[0]);
    children[0]:=nil;
  end;
(*
  finally
    for i:=0 to Length(children)-1 do
      children[i].Free;
  end;
  *)
end;

procedure TFloatExpression.Pow(s: string; var treeNode: TEvaluationTreeNode);
var i: Integer;
    brCount: Integer;
    term: TEvaluationTreeNode;
begin
  brCount:=0;
  for i:=1 to Length(s) do begin
    if (s[i]='^') and (brCount=0) then begin
      treeNode:=TPowNode.Create(nil);
      BracketsAndFuncs(LeftStr(s,i-1),term);
      treeNode.InsertComponent(term);
      BracketsAndFuncs(RightStr(s,Length(s)-i),term);
      treeNode.insertComponent(term);
      Exit;
    end;
    if s[i]='(' then inc(brCount);
    if s[i]=')' then dec(brCount);
    if brCount<0 then Raise EsyntaxErr.CreateFMT(TooManyClosingBracketsStr,[s]);
  end;
  //если выполнение дошло досюда, значит, так и не встретили символа ^
  BracketsAndFuncs(s,treeNode);
end;

procedure TFloatExpression.BracketsAndFuncs(s: string; var treeNode: TEvaluationTreeNode);
var f: string;
  i: Integer;
  temp: TEvaluationTreeNode;
begin
  if Length(s)=0 then raise ESyntaxErr.Create(EmptyStringErrStr);
  if s[Length(s)]=')' then begin
    if s[1]='(' then
      PlusMinus(MidStr(s,2,Length(s)-2),treeNode)
    else begin
      for i:=2 to Length(s)-1 do
        if s[i]='(' then begin
          f:=LeftStr(s,i-1);
          treeNode:=TMathFuncNode.Create(f,nil);
          PlusMinus(MidStr(s,i+1,Length(s)-i-1),temp);
          treeNode.InsertComponent(temp);
          Exit;
        end;
      Raise ESyntaxErr.Create(TooManyClosingBracketsStr);
    end;
  end
  else ConstsAndVars(s,treeNode);
end;

function TFloatExpression.getValue: Real;
begin
  if fchanged then MakeEvaluationTree;
  if fCorrect then begin
    if Assigned(fEvaluationTreeRoot) then begin
      if fworking then Raise Exception.CreateFMT(CircularReferenceErrStr,[fstring]);
      fworking:=true;
      Result:=fEvaluationTreeRoot.getValue;
      fworking:=false;
    end
    else Raise Exception.Create(EmptyEvaluationTreeErrStr);
  end
  else Raise Exception.Create(fLastErrorMsg);
end;

procedure TFloatExpression.MakeEvaluationTree;
begin
  FreeAndNil(fEvaluationTreeRoot);  //все дерево целиком сносится
  try
    PlusMinus(fstring,fEvaluationTreeRoot);
    fcorrect:=true;
    fIndependent:=fEvaluationTreeRoot.isIndependent;
  except
    on Ex: ESyntaxErr do begin
      fLastErrorMsg:=Ex.message;
      fcorrect:=false;
    end;
    else
      raise;
  end;
  fchanged:=false;
end;

function TFloatExpression.getVariantValue: Variant;
begin
  Result:=getValue;
end;

(*
    TStandAloneFloatExpression
                                  *)
constructor TStandAloneFloatExpression.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  SetSubComponent(false);
end;

initialization
  RegisterClasses([TFloatExpression, TStandAloneFloatExpression]);

end.

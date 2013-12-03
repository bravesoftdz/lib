// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Cautious_Edit.pas' rev: 6.00

#ifndef Cautious_EditHPP
#define Cautious_EditHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Contnrs.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Cautious_edit
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TFloatEdit;
class PASCALIMPLEMENTATION TFloatEdit : public Stdctrls::TEdit 
{
	typedef Stdctrls::TEdit inherited;
	
private:
	Controls::TControl* fControlToDisable;
	double __fastcall get_value(void);
	void __fastcall set_value(double value);
	void __fastcall SetControlToDisable(Controls::TControl* value);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Messages::TMessage &Message);
	
public:
	virtual void __fastcall Notification(Classes::TComponent* aComponent, Classes::TOperation operation);
	__fastcall virtual TFloatEdit(Classes::TComponent* Owner);
	DYNAMIC void __fastcall Change(void);
	
__published:
	__property double value = {read=get_value, write=set_value};
	__property Controls::TControl* ControlToDisable = {read=fControlToDisable, write=SetControlToDisable};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TFloatEdit(HWND ParentWindow) : Stdctrls::TEdit(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TFloatEdit(void) { }
	#pragma option pop
	
};


class DELPHICLASS TIntegerEdit;
class PASCALIMPLEMENTATION TIntegerEdit : public Stdctrls::TEdit 
{
	typedef Stdctrls::TEdit inherited;
	
private:
	Controls::TControl* fControlToDisable;
	int __fastcall get_value(void);
	void __fastcall set_value(int value);
	void __fastcall SetControlToDisable(Controls::TControl* value);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Messages::TMessage &Message);
	
public:
	virtual void __fastcall Notification(Classes::TComponent* aComponent, Classes::TOperation operation);
	__fastcall virtual TIntegerEdit(Classes::TComponent* Owner);
	DYNAMIC void __fastcall Change(void);
	
__published:
	__property int value = {read=get_value, write=set_value, nodefault};
	__property Controls::TControl* ControlToDisable = {read=fControlToDisable, write=SetControlToDisable};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TIntegerEdit(HWND ParentWindow) : Stdctrls::TEdit(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TIntegerEdit(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDisablingCheckBox;
class PASCALIMPLEMENTATION TDisablingCheckBox : public Stdctrls::TCheckBox 
{
	typedef Stdctrls::TCheckBox inherited;
	
private:
	Controls::TControl* fControlToDisable;
	void __fastcall SetControlToDisable(Controls::TControl* value);
	
public:
	virtual void __fastcall Notification(Classes::TComponent* aComponent, Classes::TOperation operation);
	DYNAMIC void __fastcall Click(void);
	
__published:
	__property Controls::TControl* ControlToDisable = {read=fControlToDisable, write=SetControlToDisable};
public:
	#pragma option push -w-inl
	/* TCustomCheckBox.Create */ inline __fastcall virtual TDisablingCheckBox(Classes::TComponent* AOwner) : Stdctrls::TCheckBox(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDisablingCheckBox(HWND ParentWindow) : Stdctrls::TCheckBox(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TDisablingCheckBox(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDisablingRadioButton;
class PASCALIMPLEMENTATION TDisablingRadioButton : public Stdctrls::TRadioButton 
{
	typedef Stdctrls::TRadioButton inherited;
	
private:
	Controls::TControl* fControlToDisable;
	void __fastcall CautiousChange(void);
	void __fastcall SetControlToDisable(Controls::TControl* value);
	MESSAGE void __fastcall BMSetCheck(Messages::TMessage &Message);
	
public:
	virtual void __fastcall Notification(Classes::TComponent* aComponent, Classes::TOperation operation);
	
__published:
	__property Controls::TControl* ControlToDisable = {read=fControlToDisable, write=SetControlToDisable};
public:
	#pragma option push -w-inl
	/* TRadioButton.Create */ inline __fastcall virtual TDisablingRadioButton(Classes::TComponent* AOwner) : Stdctrls::TRadioButton(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDisablingRadioButton(HWND ParentWindow) : Stdctrls::TRadioButton(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TDisablingRadioButton(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDisablingGroupBox;
class PASCALIMPLEMENTATION TDisablingGroupBox : public Stdctrls::TGroupBox 
{
	typedef Stdctrls::TGroupBox inherited;
	
private:
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Messages::TMessage &Message);
public:
	#pragma option push -w-inl
	/* TCustomGroupBox.Create */ inline __fastcall virtual TDisablingGroupBox(Classes::TComponent* AOwner) : Stdctrls::TGroupBox(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TDisablingGroupBox(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDisablingGroupBox(HWND ParentWindow) : Stdctrls::TGroupBox(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Cautious_edit */
using namespace Cautious_edit;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cautious_Edit

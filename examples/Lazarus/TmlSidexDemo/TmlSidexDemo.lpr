program TmlSidexDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, TmlSidexDemo_Main,
  uSidexLib, uTMLCore,
  thrDataProcessing, TmlSidexDemo_Logging;

{$IFDEF WINDOWS}
  {$R TmlSidexDemo.res}
{$ELSE}
  {$IFNDEF DARWIN}
    {$R *.res}
  {$ENDIF}
{$ENDIF}

begin
  Application.Title := 'TML-Sidex-Demo';
  Application.Initialize;

  uSidexLib.sidexUseExceptions := false;
  uTMLCore.tmlUseExceptions    := false;

  Application.CreateForm(TTmlSidexDemo_Form, TmlSidexDemo_Form);
  Application.Run;
  Application.MainForm.Free;

  uTMLCore.tmlUseExceptions    := true;
  uSidexLib.sidexUseExceptions := true;
end.


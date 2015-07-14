{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit TMLLazarusPkg; 

interface

uses
    uTMLErrors, pedTMLCommands, uTMLRegister, uSidexLib, uTMLCore, 
  uTMLClasses, uSidexVariant, cSidexDocument, uTMLTypes, uSidexTypes, 
  uSidexErrors, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('pedTMLCommands', @pedTMLCommands.Register); 
  RegisterUnit('uTMLRegister', @uTMLRegister.Register); 
end; 

initialization
  RegisterPackage('TMLLazarusPkg', @Register); 
end.

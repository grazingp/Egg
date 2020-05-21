{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit egg;

{$warn 5023 off : no warning about unused units}
interface

uses
  EggCustomControl, EggStrUtils, EggDateUtils, EggFileUtils, EggPathUtils, 
  EggEnvUtils, EggActivityDiagram, EggGraphicUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('EggActivityDiagram', @EggActivityDiagram.Register);
end;

initialization
  RegisterPackage('egg', @Register);
end.

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codetoolbarexplorer;

{$warn 5023 off : no warning about unused units}
interface

uses
    ToolbarExplorerPlugin, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ToolbarExplorerPlugin', @ToolbarExplorerPlugin.Register);
end;

initialization
  RegisterPackage('codetoolbarexplorer', @Register);
end.

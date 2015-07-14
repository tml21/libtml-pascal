(*
 *  libTML:  A BEEP based Messaging Suite
 *  Copyright (C) 2015 wobe-systems GmbH
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this program; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 *  02111-1307 USA
 *
 *  You may find a copy of the license under this software is released
 *  at COPYING file. This is LGPL software: you are welcome to develop
 *  proprietary applications using this library without any royalty or
 *  fee but returning back any change, improvement or addition in the
 *  form of source code, project image, documentation patches, etc.
 *
 *  For commercial support on build TML enabled solutions contact us:
 *
 * Contributors:
 *    wobe-systems GmbH
 *    support@libtml.org
 *)

//------------------------------------------------------------------------------

unit uTMLRegister;

//------------------------------------------------------------------------------

interface

procedure Register;

//------------------------------------------------------------------------------

implementation

uses
  Classes
{$IFDEF FPC}
  , ComponentEditors
  , LResources
{$ELSE}
  , DesignIntf
  , DesignEditors
  , VCLEditors
{$ENDIF}
  , uTMLClasses;

//------------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('TML', [TTMLProfile,TTMLCore]);
  RegisterNoIcon([TTMLCommand])
end;

//------------------------------------------------------------------------------

initialization

{$IFDEF FPC}
  {$I TMLLazarusPkg.lrs}
{$ENDIF}

//------------------------------------------------------------------------------

end.


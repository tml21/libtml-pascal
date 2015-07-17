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
 *  Homepage:
 *    http://www.libtml.org
 *
 *  For professional support contact us:
 *
 *    wobe-systems GmbH
 *    support@libtml.org
 *)

//------------------------------------------------------------------------------

unit uSidexLib;

{$if not defined(FPC)}
  {$if CompilerVersion >= 25}
    {$LEGACYIFEND ON}
  {$ifend}
  {$if CompilerVersion >= 24}
    {$define USE_NAMESPACES}
  {$ifend}
{$ifend}

{$if defined(FPC)}
  {$mode objfpc}{$H+}
{$ifend}

//------------------------------------------------------------------------------

interface

uses
  {$if defined(FPC)}
    Interfaces,
    dynlibs,
  {$ifend}
  {$if defined(Windows) or defined(MSWINDOWS)}
    Windows,
  {$ifend}
  {$if defined(ANDROID)}
    System.IOUtils,
  {$ifend}
  SysUtils, uSidexTypes;

//------------------------------------------------------------------------------

const
  SidexLibName    = 'sidex';
  SidexLibVersion = '11';

  {$if defined(Unix)}
    SIDEX_DLLHANDLE_NULL = nil;
  {$else}
    SIDEX_DLLHANDLE_NULL = 0;
  {$ifend}

  {$if defined(Unix) or defined(ANDROID)}
    SidexLibPrefix = 'lib';
  {$else}
    SidexLibPrefix = '';
  {$ifend}

  {$if defined(CPU64) or defined(CPUX64)}
    SidexLibMiddle = '64-';
  {$else}
    SidexLibMiddle = '';
  {$ifend}

  {$if defined(TML_DEBUG)}
    SidexLibDebug = 'd';
  {$else}
    SidexLibDebug = '';
  {$ifend}

  {$if not defined(FPC)}
    {$if defined(Unix) or defined(ANDROID)}
      SharedSuffix = 'so';
    {$else}
      SharedSuffix = 'dll';
    {$ifend}
  {$ifend}

  sidexdllname = SidexLibPrefix + SidexLibName + SidexLibMiddle +
                 SidexLibVersion + SidexLibDebug + '.' + SharedSuffix;

  {$if defined(ANDROID)}
    SidexAxlLibName   = SidexLibPrefix + 'axl'   + '.' + SharedSuffix;
    SidexIConvLibName = SidexLibPrefix + 'iconv' + '.' + SharedSuffix;
  {$ifend}

  {$if defined(UNICODE)}
    sidexStringTypeSuffix = '_W'; // UTF16 strings
  {$else}
    sidexStringTypeSuffix = '_A'; // UTF8 strings
  {$ifend}

//------------------------------------------------------------------------------

type
  {$if defined(UNICODE)}
    PSIDEX_STRING = PChar;
  {$else}
    {$if defined(FPC)}
      PSIDEX_STRING = PAnsiChar;
    {$else}
      {$if CompilerVersion >= 21}
        PSIDEX_STRING = PAnsiString;
      {$else}
        PSIDEX_STRING = PAnsiChar;
      {$ifend}
    {$ifend}
  {$ifend}

  {$if defined(Unix)}
    TSIDEX_DLLHANDLE = Pointer;
  {$else}
    TSIDEX_DLLHANDLE = THandle;
  {$ifend}

  EDLLLoadError   = class(Exception);
  EDLLImportError = class(Exception)
  public
    WrongFunc : String;
  end;

  Tsidex_Get_Version                 = procedure(out iAPI : SIDEX_INT32; out iLib : SIDEX_INT32; out sValue : PSIDEX_STRING); cdecl;
  Tsidex_Get_Copyright               = procedure(out sValue : PSIDEX_STRING; out iLength : SIDEX_INT32); cdecl;

  Tsidex_Get_Version_old             = procedure(out main : SIDEX_INT32; out sub : SIDEX_INT32); cdecl;
  Tsidex_Get_Version_Ext_old         = procedure(out main : SIDEX_INT32; out sub1 : SIDEX_INT32; out sub2 : SIDEX_INT32); cdecl;

  Tsidex_Set_Password                = function(pUserName : PSIDEX_STRING; pPassWord : PSIDEX_STRING) : SIDEX_INT32; cdecl;
  Tsidex_Create                      = function(pname : PSIDEX_STRING; var shandle : SIDEX_HANDLE): SIDEX_INT32; cdecl;
  Tsidex_Free                        = procedure(var shandle : SIDEX_HANDLE); cdecl;
  Tsidex_Clear                       = procedure(shandle : SIDEX_HANDLE); cdecl;

  Tsidex_Set_DocumentName            = function(shandle : SIDEX_HANDLE; pname : PSIDEX_STRING): SIDEX_INT32; cdecl;
  Tsidex_Get_DocumentName            = function(shandle : SIDEX_HANDLE; out pname : PSIDEX_STRING): SIDEX_INT32; cdecl;

  Tsidex_Load_Content                = function(shandle : SIDEX_HANDLE; path : PSIDEX_STRING): SIDEX_INT32; cdecl;
  Tsidex_Save_Content                = function(shandle : SIDEX_HANDLE; path : PSIDEX_STRING): SIDEX_INT32; cdecl;
  Tsidex_Set_Content                 = function(shandle : SIDEX_HANDLE; content : PSIDEX_STRING): SIDEX_INT32; cdecl;
  Tsidex_Get_Content                 = function(shandle : SIDEX_HANDLE; out content : PSIDEX_STRING; out iContentLength : SIDEX_INT32): SIDEX_INT32; cdecl;
  Tsidex_Get_Content_Length          = function(shandle : SIDEX_HANDLE; out iContentLength : SIDEX_INT32): SIDEX_INT32; cdecl;
  Tsidex_Free_Content                = procedure(aptr : PSIDEX_STRING); cdecl;

  Tsidex_GetGroups                   = function(shandle : SIDEX_HANDLE; out sGroups : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_GetKeys                     = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; out skeys : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_DeleteGroup                 = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING): SIDEX_INT32; cdecl;
  Tsidex_DeleteKey                   = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING): SIDEX_INT32; cdecl;
  Tsidex_HasGroup                    = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING): SIDEX_BOOL; cdecl;
  Tsidex_HasKey                      = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING): SIDEX_BOOL; cdecl;
  Tsidex_Merge                       = function(sBaseHandle, sMergeHandle : SIDEX_HANDLE; bOverwrite : SIDEX_BOOL; nGroup, nKey : PSIDEX_STRING): SIDEX_INT32; cdecl;

  Tsidex_None_Write                  = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING): SIDEX_INT32; cdecl;

  Tsidex_Boolean_Read                = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; out value : SIDEX_BOOL): SIDEX_INT32; cdecl;
  Tsidex_Boolean_Write               = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; value : SIDEX_BOOL): SIDEX_INT32; cdecl;

  Tsidex_Integer_Read                = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; out value : SIDEX_INT64): SIDEX_INT32; cdecl;
  Tsidex_Integer_Write               = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; value : SIDEX_INT64): SIDEX_INT32; cdecl;

  Tsidex_Float_Read                  = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; out value : SIDEX_DOUBLE): SIDEX_INT32; cdecl;
  Tsidex_Float_Write                 = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; value : SIDEX_DOUBLE): SIDEX_INT32; cdecl;

  Tsidex_DateTime_Read               = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; out value : SIDEX_VARIANT) : SIDEX_INT32; cdecl;
  Tsidex_DateTime_Write              = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; value : SIDEX_VARIANT) : SIDEX_INT32; cdecl;

  Tsidex_String_Length               = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; out iStrLength : SIDEX_INT32) : SIDEX_INT32; cdecl;
  Tsidex_String_Read                 = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; out value : PSIDEX_STRING; out iStrLength : SIDEX_INT32) : SIDEX_INT32; cdecl;
  Tsidex_String_Write                = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; value : PSIDEX_STRING) : SIDEX_INT32; cdecl;
  Tsidex_Free_ReadString             = procedure(astr : PSIDEX_STRING); cdecl;

  Tsidex_Binary_Length               = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; out iStrLength : SIDEX_INT32) : SIDEX_INT32; cdecl;
  Tsidex_Binary_Read                 = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; out value : PByte; out iStrLength : SIDEX_INT32) : SIDEX_INT32; cdecl;
  Tsidex_Binary_Write                = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; value : PByte; size : SIDEX_INT32) : SIDEX_INT32; cdecl;
  Tsidex_Free_Binary_ReadString      = procedure(abinstr : PByte); cdecl;

  Tsidex_List_Read                   = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; out value : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_List_Write                  = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; value : SIDEX_VARIANT): SIDEX_INT32; cdecl;

  Tsidex_Dict_Read                   = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; out value : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Dict_Write                  = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; value : SIDEX_VARIANT): SIDEX_INT32; cdecl;

  Tsidex_Table_Read                  = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; out value : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Table_Write                 = function(shandle : SIDEX_HANDLE; ngroup : PSIDEX_STRING; nkey : PSIDEX_STRING; value : SIDEX_VARIANT): SIDEX_INT32; cdecl;

  // ----- Variant -----

  Tsidex_Variant_DecRef              = procedure(svariant : SIDEX_VARIANT); cdecl;
  Tsidex_Variant_IncRef              = procedure(svariant : SIDEX_VARIANT); cdecl;
  Tsidex_Variant_Write               = function(shandle : SIDEX_HANDLE; ngroup, nkey : PSIDEX_STRING; svariant : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_Read                = function(shandle : SIDEX_HANDLE; ngroup, nkey : PSIDEX_STRING; out svariant : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_GetType             = function(value : SIDEX_VARIANT): SIDEX_DATA_TYPE; cdecl;
  Tsidex_Variant_Copy                = function(svariant : SIDEX_VARIANT): SIDEX_VARIANT; cdecl;

  Tsidex_Variant_New_None            = function(): SIDEX_VARIANT;
  Tsidex_Variant_New_Boolean         = function(avalue : SIDEX_BOOL)  : SIDEX_VARIANT; cdecl;
  Tsidex_Variant_New_Integer         = function(avalue : SIDEX_INT64) : SIDEX_VARIANT; cdecl;
  Tsidex_Variant_New_Float           = function(avalue : SIDEX_DOUBLE) : SIDEX_VARIANT; cdecl;
  Tsidex_Variant_New_DateTime        = function(sDateTime : PSIDEX_STRING; out variant : SIDEX_VARIANT) : SIDEX_INT32; cdecl;
  Tsidex_Variant_New_String          = function(avalue : PSIDEX_STRING; out variant : SIDEX_VARIANT) : SIDEX_INT32; cdecl;
  Tsidex_Variant_New_Binary          = function(value : PByte; iStrLength : SIDEX_INT32; out variant : SIDEX_VARIANT) : SIDEX_INT32; cdecl;
  Tsidex_Variant_New_List            = function(): SIDEX_VARIANT; cdecl;
  Tsidex_Variant_New_Dict            = function(): SIDEX_VARIANT; cdecl;
  Tsidex_Variant_New_DictBySize      = function(iSize : SIDEX_INT32): SIDEX_VARIANT; cdecl;
  Tsidex_Variant_New_Table           = function(): SIDEX_VARIANT; cdecl;

  Tsidex_Variant_None_Check          = function(avalue : SIDEX_VARIANT): SIDEX_BOOL; cdecl;
  Tsidex_Variant_Boolean_Check       = function(avalue : SIDEX_VARIANT): SIDEX_BOOL; cdecl;
  Tsidex_Variant_Integer_Check       = function(avalue : SIDEX_VARIANT): SIDEX_BOOL; cdecl;
  Tsidex_Variant_Float_Check         = function(avalue : SIDEX_VARIANT): SIDEX_BOOL; cdecl;
  Tsidex_Variant_DateTime_Check      = function(avalue : SIDEX_VARIANT): SIDEX_BOOL; cdecl;
  Tsidex_Variant_String_Check        = function(avalue : SIDEX_VARIANT): SIDEX_BOOL; cdecl;
  Tsidex_Variant_Binary_Check        = function(avalue : SIDEX_VARIANT): SIDEX_BOOL; cdecl;
  Tsidex_Variant_List_Check          = function(avalue : SIDEX_VARIANT): SIDEX_BOOL; cdecl;
  Tsidex_Variant_Dict_Check          = function(avalue : SIDEX_VARIANT): SIDEX_BOOL; cdecl;
  Tsidex_Variant_Table_Check         = function(avalue : SIDEX_VARIANT): SIDEX_BOOL; cdecl;

  Tsidex_Variant_As_Boolean          = function(avalue : SIDEX_VARIANT; out bValue : SIDEX_BOOL): SIDEX_INT32; cdecl;
  Tsidex_Variant_As_Integer          = function(avalue : SIDEX_VARIANT; out iValue : SIDEX_INT64): SIDEX_INT32; cdecl;
  Tsidex_Variant_As_Float            = function(avalue : SIDEX_VARIANT; out fValue : SIDEX_DOUBLE): SIDEX_INT32; cdecl;
  Tsidex_Variant_As_DateTime         = function(avalue : SIDEX_VARIANT; out dtValue : PSIDEX_STRING): SIDEX_INT32; cdecl;
  Tsidex_Variant_As_String           = function(avalue : SIDEX_VARIANT; out sValue : PSIDEX_STRING; out iLength : SIDEX_INT32): SIDEX_INT32; cdecl;
  Tsidex_Variant_As_String_Length    = function(avalue : SIDEX_VARIANT; out iLength : SIDEX_INT32): SIDEX_INT32; cdecl;
  Tsidex_Variant_As_Binary           = function(avalue : SIDEX_VARIANT; out sValue : PByte; out iLength : SIDEX_INT32) : SIDEX_INT32; cdecl;
  Tsidex_Variant_As_Binary_Length    = function(avalue : SIDEX_VARIANT; out iLength : SIDEX_INT32) : SIDEX_INT32; cdecl;

  Tsidex_Variant_Dict_Size           = function(avalue : SIDEX_VARIANT; out iSize : SIDEX_INT32): SIDEX_INT32; cdecl;
  Tsidex_Variant_Dict_Clear          = function(sDict : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_Dict_Get            = function(sDict : SIDEX_VARIANT; sKey : PSIDEX_STRING; out value : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_Dict_Set            = function(sDict : SIDEX_VARIANT; sKey : PSIDEX_STRING; value : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_Dict_Keys           = function(sDict : SIDEX_VARIANT; out sKeys : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_Dict_HasKey         = function(avalue : SIDEX_VARIANT; sKey : PSIDEX_STRING; out bRet : SIDEX_BOOL): SIDEX_INT32; cdecl;
  Tsidex_Variant_Dict_Delete         = function(sDict : SIDEX_VARIANT; sKey : PSIDEX_STRING): SIDEX_INT32; cdecl;
  Tsidex_Variant_Dict_First          = function(sDict : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_Dict_Next           = function(sDict : SIDEX_VARIANT; out sKey : PSIDEX_STRING; out vNext : SIDEX_VARIANT): SIDEX_INT32; cdecl;

  Tsidex_Variant_Table_Columns       = function(avalue : SIDEX_VARIANT; out iColumns : SIDEX_INT32) : SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_AddColumn     = function(sTable : SIDEX_VARIANT; sColumn : PSIDEX_STRING) : SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_DeleteColumn  = function(sTable : SIDEX_VARIANT; sColumn : PSIDEX_STRING) : SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_GetColumnName = function(sTable : SIDEX_VARIANT; index : SIDEX_INT32; out sColumn : PSIDEX_STRING) : SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_ColumnNames   = function(sDict : SIDEX_VARIANT; out varNames : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_HasColumn     = function(avalue : SIDEX_VARIANT; sColumnName : PSIDEX_STRING; out bRet : SIDEX_BOOL): SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_Rows          = function(avalue : SIDEX_VARIANT; out iRows : SIDEX_INT32) : SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_AddRow        = function(sTable : SIDEX_VARIANT; out iRowIdx : SIDEX_INT32): SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_DeleteRow     = function(sTable : SIDEX_VARIANT; rowIndex : SIDEX_INT32): SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_DeleteRows    = function(sTable : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_GetRow        = function(sTable : SIDEX_VARIANT; rowIndex : SIDEX_INT32; out row : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_GetField      = function(sTable : SIDEX_VARIANT; rowIndex : SIDEX_INT32; sColumnName : PSIDEX_STRING; out value : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_Table_SetField      = function(sTable : SIDEX_VARIANT; rowIndex : SIDEX_INT32; sColumnName : PSIDEX_STRING; value : SIDEX_VARIANT): SIDEX_INT32; cdecl;

  Tsidex_Variant_List_Size           = function(avalue : SIDEX_VARIANT; out iSize : SIDEX_INT32): SIDEX_INT32; cdecl;
  Tsidex_Variant_List_Clear          = function(sList : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_List_Get            = function(sList : SIDEX_VARIANT; index : SIDEX_INT32; out refElement : SIDEX_VARIANT): SIDEX_INT32; cdecl;
  Tsidex_Variant_List_Set            = function(sList : SIDEX_VARIANT;  value : SIDEX_VARIANT; pos : SIDEX_INT32): SIDEX_INT32; cdecl;
  Tsidex_Variant_List_Append         = function(sList : SIDEX_VARIANT;  value : SIDEX_VARIANT; out apos : SIDEX_INT32): SIDEX_INT32; cdecl;
  Tsidex_Variant_List_Insert         = function(sList : SIDEX_VARIANT;  value : SIDEX_VARIANT; pos : SIDEX_INT32): SIDEX_INT32; cdecl;
  Tsidex_Variant_List_DeleteItem     = function(sList : SIDEX_VARIANT; pos : SIDEX_INT32): SIDEX_INT32; cdecl;

  Tsidex_Variant_String_GetFormat    = function(avalue : SIDEX_VARIANT; out sformat : PSIDEX_STRING): SIDEX_INT32; cdecl;
  Tsidex_Variant_String_SetFormat    = function(avalue : SIDEX_VARIANT; sformat : PSIDEX_STRING): SIDEX_INT32; cdecl;

//------------------------------------------------------------------------------

var
  sidex_Get_Version                 : Tsidex_Get_Version                 = nil;
  sidex_Get_Copyright               : Tsidex_Get_Copyright               = nil;

  sidex_Get_Version_old             : Tsidex_Get_Version_old             = nil;
  sidex_Get_Version_Ext_old         : Tsidex_Get_Version_Ext_old         = nil;

  sidex_Set_Password                : Tsidex_Set_Password                = nil;
  sidex_Create                      : Tsidex_Create                      = nil;
  sidex_Free                        : Tsidex_Free                        = nil;
  sidex_Clear                       : Tsidex_Clear                       = nil;

  sidex_Set_DocumentName            : Tsidex_Set_DocumentName            = nil;
  sidex_Get_DocumentName            : Tsidex_Get_DocumentName            = nil;

  sidex_Load_Content                : Tsidex_Load_Content                = nil;
  sidex_Save_Content                : Tsidex_Save_Content                = nil;
  sidex_Set_Content                 : Tsidex_Set_Content                 = nil;
  sidex_Get_Content                 : Tsidex_Get_Content                 = nil;
  sidex_Get_Content_Length          : Tsidex_Get_Content_Length          = nil;
  sidex_Free_Content                : Tsidex_Free_Content                = nil;

  sidex_None_Write                  : Tsidex_None_Write                  = nil;

  sidex_Boolean_Read                : Tsidex_Boolean_Read                = nil;
  sidex_Boolean_Write               : Tsidex_Boolean_Write               = nil;

  sidex_Integer_Read                : Tsidex_Integer_Read                = nil;
  sidex_Integer_Write               : Tsidex_Integer_Write               = nil;

  sidex_Float_Read                  : Tsidex_Float_Read                  = nil;
  sidex_Float_Write                 : Tsidex_Float_Write                 = nil;

  sidex_DateTime_Read               : Tsidex_DateTime_Read               = nil;
  sidex_DateTime_Write              : Tsidex_DateTime_Write              = nil;

  sidex_String_Length               : Tsidex_String_Length               = nil;
  sidex_String_Read                 : Tsidex_String_Read                 = nil;
  sidex_String_Write                : Tsidex_String_Write                = nil;
  sidex_Free_ReadString             : Tsidex_Free_ReadString             = nil;

  sidex_Binary_Length               : Tsidex_Binary_Length               = nil;
  sidex_Binary_Read                 : Tsidex_Binary_Read                 = nil;
  sidex_Binary_Write                : Tsidex_Binary_Write                = nil;
  sidex_Free_Binary_ReadString      : Tsidex_Free_Binary_ReadString      = nil;

  sidex_List_Read                   : Tsidex_List_Read                   = nil;
  sidex_List_Write                  : Tsidex_List_Write                  = nil;

  sidex_Dict_Read                   : Tsidex_Dict_Read                   = nil;
  sidex_Dict_Write                  : Tsidex_Dict_Write                  = nil;

  sidex_Table_Read                  : Tsidex_Table_Read                  = nil;
  sidex_Table_Write                 : Tsidex_Table_Write                 = nil;

  sidex_GetGroups                   : Tsidex_GetGroups                   = nil;
  sidex_GetKeys                     : Tsidex_GetKeys                     = nil;
  sidex_DeleteGroup                 : Tsidex_DeleteGroup                 = nil;
  sidex_DeleteKey                   : Tsidex_DeleteKey                   = nil;
  sidex_HasGroup                    : Tsidex_HasGroup                    = nil;
  sidex_HasKey                      : Tsidex_HasKey                      = nil;
  sidex_Merge                       : Tsidex_Merge                       = nil;

  // ----- Variant -----

  sidex_Variant_DecRef              : Tsidex_Variant_DecRef              = nil;
  sidex_Variant_IncRef              : Tsidex_Variant_IncRef              = nil;
  sidex_Variant_Copy                : Tsidex_Variant_Copy                = nil;

  sidex_Variant_GetType             : Tsidex_Variant_GetType             = nil;

  sidex_Variant_Write               : Tsidex_Variant_Write               = nil;
  sidex_Variant_Read                : Tsidex_Variant_Read                = nil;

  sidex_Variant_New_None            : Tsidex_Variant_New_None            = nil;
  sidex_Variant_New_Boolean         : Tsidex_Variant_New_Boolean         = nil;
  sidex_Variant_New_Integer         : Tsidex_Variant_New_Integer         = nil;
  sidex_Variant_New_Float           : Tsidex_Variant_New_Float           = nil;
  sidex_Variant_New_String          : Tsidex_Variant_New_String          = nil;
  sidex_Variant_New_DateTime        : Tsidex_Variant_New_DateTime        = nil;
  sidex_Variant_New_Binary          : Tsidex_Variant_New_Binary          = nil;
  sidex_Variant_New_List            : Tsidex_Variant_New_List            = nil;
  sidex_Variant_New_Dict            : Tsidex_Variant_New_Dict            = nil;
  sidex_Variant_New_DictBySize      : Tsidex_Variant_New_DictBySize      = nil;
  sidex_Variant_New_Table           : Tsidex_Variant_New_Table           = nil;

  sidex_Variant_None_Check          : Tsidex_Variant_None_Check          = nil;
  sidex_Variant_Boolean_Check       : Tsidex_Variant_Boolean_Check       = nil;
  sidex_Variant_Integer_Check       : Tsidex_Variant_Integer_Check       = nil;
  sidex_Variant_Float_Check         : Tsidex_Variant_Float_Check         = nil;
  sidex_Variant_String_Check        : Tsidex_Variant_String_Check        = nil;
  sidex_Variant_Binary_Check        : Tsidex_Variant_Binary_Check        = nil;
  sidex_Variant_DateTime_Check      : Tsidex_Variant_DateTime_Check      = nil;
  sidex_Variant_List_Check          : Tsidex_Variant_List_Check          = nil;
  sidex_Variant_Dict_Check          : Tsidex_Variant_Dict_Check          = nil;
  sidex_Variant_Table_Check         : Tsidex_Variant_Table_Check         = nil;

  sidex_Variant_As_Boolean          : Tsidex_Variant_As_Boolean          = nil;
  sidex_Variant_As_Integer          : Tsidex_Variant_As_Integer          = nil;
  sidex_Variant_As_Float            : Tsidex_Variant_As_Float            = nil;
  sidex_Variant_As_String           : Tsidex_Variant_As_String           = nil;
  sidex_Variant_As_String_Length    : Tsidex_Variant_As_String_Length    = nil;
  sidex_Variant_As_DateTime         : Tsidex_Variant_As_DateTime         = nil;
  sidex_Variant_As_Binary           : Tsidex_Variant_As_Binary           = nil;
  sidex_Variant_As_Binary_Length    : Tsidex_Variant_As_Binary_Length    = nil;

  sidex_Variant_List_Size           : Tsidex_Variant_List_Size           = nil;
  sidex_Variant_List_Clear          : Tsidex_Variant_List_Clear          = nil;
  sidex_Variant_List_Append         : Tsidex_Variant_List_Append         = nil;
  sidex_Variant_List_Set            : Tsidex_Variant_List_Set            = nil;
  sidex_Variant_List_Insert         : Tsidex_Variant_List_Insert         = nil;
  sidex_Variant_List_DeleteItem     : Tsidex_Variant_List_DeleteItem     = nil;
  sidex_Variant_List_Get            : Tsidex_Variant_List_Get            = nil;

  sidex_Variant_Dict_Size           : Tsidex_Variant_Dict_Size           = nil;
  sidex_Variant_Dict_Clear          : Tsidex_Variant_Dict_Clear          = nil;
  sidex_Variant_Dict_Keys           : Tsidex_Variant_Dict_Keys           = nil;
  sidex_Variant_Dict_HasKey         : Tsidex_Variant_Dict_HasKey         = nil;
  sidex_Variant_Dict_Set            : Tsidex_Variant_Dict_Set            = nil;
  sidex_Variant_Dict_Delete         : Tsidex_Variant_Dict_Delete         = nil;
  sidex_Variant_Dict_Get            : Tsidex_Variant_Dict_Get            = nil;
  sidex_Variant_Dict_First          : Tsidex_Variant_Dict_First          = nil;
  sidex_Variant_Dict_Next           : Tsidex_Variant_Dict_Next           = nil;

  sidex_Variant_Table_AddRow        : Tsidex_Variant_Table_AddRow        = nil;
  sidex_Variant_Table_DeleteRow     : Tsidex_Variant_Table_DeleteRow     = nil;
  sidex_Variant_Table_GetRow        : Tsidex_Variant_Table_GetRow        = nil;
  sidex_Variant_Table_GetField      : Tsidex_Variant_Table_GetField      = nil;
  sidex_Variant_Table_SetField      : Tsidex_Variant_Table_SetField      = nil;
  sidex_Variant_Table_AddColumn     : Tsidex_Variant_Table_AddColumn     = nil;
  sidex_Variant_Table_DeleteColumn  : Tsidex_Variant_Table_DeleteColumn  = nil;
  sidex_Variant_Table_GetColumnName : Tsidex_Variant_Table_GetColumnName = nil;
  sidex_Variant_Table_DeleteRows    : Tsidex_Variant_Table_DeleteRows    = nil;
  sidex_Variant_Table_Rows          : Tsidex_Variant_Table_Rows          = nil;
  sidex_Variant_Table_Columns       : Tsidex_Variant_Table_Columns       = nil;
  sidex_Variant_Table_ColumnNames   : Tsidex_Variant_Table_ColumnNames   = nil;
  sidex_Variant_Table_HasColumn     : Tsidex_Variant_Table_HasColumn     = nil;

  sidex_Variant_String_GetFormat    : Tsidex_Variant_String_GetFormat    = nil;
  sidex_Variant_String_SetFormat    : Tsidex_Variant_String_SetFormat    = nil;

//------------------------------------------------------------------------------

var
  sidexUseExceptions   : Boolean;

  sidexDesignCopyright : string;
  sidexDesignVersion   : string;

//------------------------------------------------------------------------------

function sidex_loaded    : Boolean;

function sidex_Copyright : string;
function sidex_Version   : string;

//------------------------------------------------------------------------------

implementation

uses
  {$if defined(Unix)}
    dl,
  {$elseif defined(ANDROID)}
    Posix.Dlfcn,
  {$ifend}
  {$if defined(FPC)}
    LCLType,
  {$ifend}
  {$if defined(USE_NAMESPACES)}
    {$if defined(ANDROID)}
      FMX.Dialogs,
      FMX.Forms,
    {$else}
      Vcl.Dialogs,
      Vcl.Forms,
    {$ifend}
    System.UITypes;
  {$else}
    Dialogs,
    Forms;
  {$ifend}

//------------------------------------------------------------------------------

{$if not defined(ANDROID)}
const
  // keep the same list in uTMLCore
  sidexExeDesigners : array[0..2] of string = ( 'bds.exe',
                                                'lazarus.exe',
                                                'lazarus' );
{$ifend}

var
  sidex_dllHandle : TSIDEX_DLLHANDLE;
  sidexLoaded     : Boolean;
  sidexDesigning  : Boolean;

  {$if defined(ANDROID)}
    sidex_dllHandle_AXL   : TSIDEX_DLLHANDLE;
    sidex_dllHandle_IConv : TSIDEX_DLLHANDLE;
  {$else}
    sidexLoopIdx     : Integer;
    sidexExeFileName : string;
  {$ifend}

//------------------------------------------------------------------------------

function UnloadDll_Sidex: Boolean;
begin
  if sidex_dllHandle <> SIDEX_DLLHANDLE_NULL then
  begin
    {$if defined(Unix)}
      Result := (dlclose(sidex_dllHandle) = 0);
    {$elseif defined(FPC)}
      Result := UnloadLibrary(sidex_dllHandle);
    {$else}
      Result := FreeLibrary(sidex_dllHandle);
    {$ifend}
    sidex_dllHandle := SIDEX_DLLHANDLE_NULL;

    {$if defined(ANDROID)}
      if sidex_dllHandle_IConv <> SIDEX_DLLHANDLE_NULL then
      begin
        FreeLibrary(sidex_dllHandle_IConv);
        sidex_dllHandle_IConv := SIDEX_DLLHANDLE_NULL;
      end;
      if sidex_dllHandle_AXL <> SIDEX_DLLHANDLE_NULL then
      begin
        FreeLibrary(sidex_dllHandle_AXL);
        sidex_dllHandle_AXL := SIDEX_DLLHANDLE_NULL;
      end;
    {$ifend}
  end
  else Result := true;
end;

function LoadDll_Sidex(adllname : string): Boolean;
var
  errMsg: string;
begin
  UnloadDll_Sidex;
  {$if defined(Unix)}
    sidex_dllHandle := dlopen(PChar(adllname), RTLD_LAZY);
  {$else}
    {$if defined(ANDROID)}
      sidex_dllHandle_IConv := LoadLibrary(PChar(TPath.Combine(TPath.GetLibraryPath, SidexIConvLibName)));
      sidex_dllHandle_AXL   := LoadLibrary(PChar(TPath.Combine(TPath.GetLibraryPath, SidexAxlLibName)));
      adllname              := TPath.Combine(TPath.GetLibraryPath, adllname);
    {$ifend}
    sidex_dllHandle := LoadLibrary(PChar(adllname));
  {$ifend}
  Result := (sidex_dllHandle <> SIDEX_DLLHANDLE_NULL);
  if not Result then
  begin
    if sidexDesigning then
    begin
      sidexDesignCopyright := 'SIDEX-DLL not found!';
      sidexDesignVersion   := sidexDesignCopyright;
    end
    else if sidexUseExceptions then
    begin
      {$if defined(Unix) or defined(ANDROID)}
        errMsg := #13 + string(dlerror());
      {$else}
        errMsg := '';
      {$ifend}
      raise Exception.Create('cannot load ' + adllname + errMsg);
    end;
  end;
end;

function ImportDllFunction(dllhandle : TSIDEX_DLLHANDLE; funcname : string): Pointer;
var
  E : EDLLImportError;
  errMsg: string;
begin
  {$if defined(Unix)}
    Result := dlsym(dllhandle, PChar(funcname));
  {$else}
    Result := GetProcAddress(dllhandle, PChar(funcname));
  {$ifend}
  if (Result = nil) and not sidexDesigning then
  begin
    {$if defined(Unix) or defined(ANDROID)}
      errMsg := #13 + string(dlerror());
    {$else}
      errMsg := '';
    {$ifend}
    E := EDllImportError.CreateFmt('could not find dll function "%s"%s',
                                   [ funcname, errMsg ]);
    E.WrongFunc := funcname;
    raise E;
  end;
end;

function LoadFunctions_Info_Sidex(dllhandle : TSIDEX_DLLHANDLE;
                                  useExceptions : Boolean = false): Boolean;
begin
  Result := false;
  try
    try
      sidex_Get_Version := Tsidex_Get_Version(ImportDllFunction(dllhandle,'sidex_Get_Version'+sidexStringTypeSuffix));
    except
      on E: EDLLImportError do
      begin
        sidex_Get_Version := nil;
        try
          sidex_Get_Version_old := Tsidex_Get_Version_old(ImportDllFunction(dllhandle,'sidex_Get_Version'));
          try
            sidex_Get_Version_Ext_old := Tsidex_Get_Version_Ext_old(ImportDllFunction(dllhandle,'sidex_Get_Version_Ext'));
          except
            on E: EDLLImportError do
            begin
              sidex_Get_Version_Ext_old := nil;
            end;
          end;
        except
          on E: EDLLImportError do
          begin
            sidex_Get_Version_old     := nil;
            sidex_Get_Version_Ext_old := nil;
          end;
        end;
        if not Assigned(sidex_Get_Version_old) then raise;
      end;
    end;
    if sidexDesigning then
    begin
      if not Assigned(sidex_Get_Version) then
      begin
        sidexDesignVersion := 'Error: DLL function "sidex_Get_Version' +
                              sidexStringTypeSuffix + '" not found!';
      end
      else sidexDesignVersion := sidex_Version();
    end;

    try
      sidex_Get_Copyright := Tsidex_Get_Copyright(ImportDllFunction(dllhandle,'sidex_Get_Copyright'+sidexStringTypeSuffix));
    except
      on E: EDLLImportError do
      begin
        sidex_Get_Copyright := nil;
      end;
    end;
    if sidexDesigning then
    begin
      if not Assigned(sidex_Get_Copyright) then
      begin
        sidexDesignCopyright := 'Error: DLL function "sidex_Get_Copyright' +
                                sidexStringTypeSuffix + '" not found!';
      end
      else sidexDesignCopyright := sidex_Copyright();
    end;

    Result := true;
  except
    on E: EDLLImportError do
    begin
      if useExceptions then raise;
    end;
  end;
end;

function LoadFunctions_Sidex(dllhandle : TSIDEX_DLLHANDLE): Boolean;
begin
  Result := false;
  try
    sidex_Set_Password                := Tsidex_Set_Password(ImportDllFunction(dllhandle,'sidex_Set_Password'+sidexStringTypeSuffix));
    sidex_Create                      := Tsidex_Create(ImportDllFunction(dllhandle,'sidex_Create'+sidexStringTypeSuffix));
    sidex_Free                        := Tsidex_Free(ImportDllFunction(dllhandle,'sidex_Free'));

    sidex_Set_DocumentName            := Tsidex_Set_DocumentName(ImportDllFunction(dllhandle,'sidex_Set_DocumentName'+sidexStringTypeSuffix));
    sidex_Get_DocumentName            := Tsidex_Get_DocumentName(ImportDllFunction(dllhandle,'sidex_Get_DocumentName'+sidexStringTypeSuffix));
    sidex_Clear                       := Tsidex_Clear(ImportDllFunction(dllhandle,'sidex_Clear'));

    sidex_Load_Content                := Tsidex_Load_Content(ImportDllFunction(dllhandle,'sidex_Load_Content'+sidexStringTypeSuffix));
    sidex_Set_Content                 := Tsidex_Set_Content(ImportDllFunction(dllhandle,'sidex_Set_Content'+sidexStringTypeSuffix));
    sidex_Save_Content                := Tsidex_Save_Content(ImportDllFunction(dllhandle,'sidex_Save_Content'+sidexStringTypeSuffix));
    sidex_Get_Content                 := Tsidex_Get_Content(ImportDllFunction(dllhandle,'sidex_Get_Content'+sidexStringTypeSuffix));
    sidex_Get_Content_Length          := Tsidex_Get_Content_Length(ImportDllFunction(dllhandle,'sidex_Get_Content_Length'));
    sidex_Free_Content                := Tsidex_Free_Content(ImportDllFunction(dllhandle,'sidex_Free_Content'+sidexStringTypeSuffix));

    sidex_None_Write                  := Tsidex_None_Write(ImportDllFunction(dllhandle,'sidex_None_Write'+sidexStringTypeSuffix));

    sidex_Boolean_Read                := Tsidex_Boolean_Read(ImportDllFunction(dllhandle,'sidex_Boolean_Read'+sidexStringTypeSuffix));
    sidex_Boolean_Write               := Tsidex_Boolean_Write(ImportDllFunction(dllhandle,'sidex_Boolean_Write'+sidexStringTypeSuffix));

    sidex_Integer_Read                := Tsidex_Integer_Read(ImportDllFunction(dllhandle,'sidex_Integer_Read'+sidexStringTypeSuffix));
    sidex_Integer_Write               := Tsidex_Integer_Write(ImportDllFunction(dllhandle,'sidex_Integer_Write'+sidexStringTypeSuffix));

    sidex_Float_Read                  := Tsidex_Float_Read(ImportDllFunction(dllhandle,'sidex_Float_Read'+sidexStringTypeSuffix));
    sidex_Float_Write                 := Tsidex_Float_Write(ImportDllFunction(dllhandle,'sidex_Float_Write'+sidexStringTypeSuffix));

    sidex_DateTime_Read               := Tsidex_DateTime_Read(ImportDllFunction(dllhandle,'sidex_DateTime_Read'+sidexStringTypeSuffix));
    sidex_DateTime_Write              := Tsidex_DateTime_Write(ImportDllFunction(dllhandle,'sidex_DateTime_Write'+sidexStringTypeSuffix));

    sidex_String_Length               := Tsidex_String_Length(ImportDllFunction(dllhandle,'sidex_String_Length'+sidexStringTypeSuffix));
    sidex_String_Read                 := Tsidex_String_Read(ImportDllFunction(dllhandle,'sidex_String_Read'+sidexStringTypeSuffix));
    sidex_String_Write                := Tsidex_String_Write(ImportDllFunction(dllhandle,'sidex_String_Write'+sidexStringTypeSuffix));
    sidex_Free_ReadString             := Tsidex_Free_ReadString(ImportDllFunction(dllhandle,'sidex_Free_ReadString'+sidexStringTypeSuffix));

    sidex_Binary_Length               := Tsidex_Binary_Length(ImportDllFunction(dllhandle,'sidex_Binary_Length'+sidexStringTypeSuffix));
    sidex_Binary_Read                 := Tsidex_Binary_Read(ImportDllFunction(dllhandle,'sidex_Binary_Read'+sidexStringTypeSuffix));
    sidex_Binary_Write                := Tsidex_Binary_Write(ImportDllFunction(dllhandle,'sidex_Binary_Write'+sidexStringTypeSuffix));
    sidex_Free_Binary_ReadString      := Tsidex_Free_Binary_ReadString(ImportDllFunction(dllhandle,'sidex_Free_Binary_ReadString'));

    sidex_List_Read                   := Tsidex_List_Read(ImportDllFunction(dllhandle,'sidex_List_Read'+sidexStringTypeSuffix));
    sidex_List_Write                  := Tsidex_List_Write(ImportDllFunction(dllhandle,'sidex_List_Write'+sidexStringTypeSuffix));

    sidex_Dict_Read                   := Tsidex_Dict_Read(ImportDllFunction(dllhandle,'sidex_Dict_Read'+sidexStringTypeSuffix));
    sidex_Dict_Write                  := Tsidex_Dict_Write(ImportDllFunction(dllhandle,'sidex_Dict_Write'+sidexStringTypeSuffix));

    sidex_Table_Read                  := Tsidex_Table_Read(ImportDllFunction(dllhandle,'sidex_Table_Read'+sidexStringTypeSuffix));
    sidex_Table_Write                 := Tsidex_Table_Write(ImportDllFunction(dllhandle,'sidex_Table_Write'+sidexStringTypeSuffix));

    sidex_GetGroups                   := Tsidex_GetGroups(ImportDllFunction(dllhandle,'sidex_GetGroups'));
    sidex_GetKeys                     := Tsidex_GetKeys(ImportDllFunction(dllhandle,'sidex_GetKeys'+sidexStringTypeSuffix));
    sidex_DeleteGroup                 := Tsidex_DeleteGroup(ImportDllFunction(dllhandle,'sidex_DeleteGroup'+sidexStringTypeSuffix));
    sidex_DeleteKey                   := Tsidex_DeleteKey(ImportDllFunction(dllhandle,'sidex_DeleteKey'+sidexStringTypeSuffix));
    sidex_HasGroup                    := Tsidex_HasGroup(ImportDllFunction(dllhandle,'sidex_HasGroup'+sidexStringTypeSuffix));
    sidex_HasKey                      := Tsidex_HasKey(ImportDllFunction(dllhandle,'sidex_HasKey'+sidexStringTypeSuffix));
    sidex_Merge                       := Tsidex_Merge(ImportDllFunction(dllhandle,'sidex_Merge'+sidexStringTypeSuffix));

    // ----- Variant -----

    sidex_Variant_DecRef              := Tsidex_Variant_DecRef(ImportDllFunction(dllhandle,'sidex_Variant_DecRef'));
    sidex_Variant_IncRef              := Tsidex_Variant_IncRef(ImportDllFunction(dllhandle,'sidex_Variant_IncRef'));

    sidex_Variant_Copy                := Tsidex_Variant_Copy(ImportDllFunction(dllhandle,'sidex_Variant_Copy'));

    sidex_Variant_GetType             := Tsidex_Variant_GetType(ImportDllFunction(dllhandle,'sidex_Variant_GetType'));

    sidex_Variant_Write               := Tsidex_Variant_Write(ImportDllFunction(dllhandle,'sidex_Variant_Write'+sidexStringTypeSuffix));
    sidex_Variant_Read                := Tsidex_Variant_Read(ImportDllFunction(dllhandle,'sidex_Variant_Read'+sidexStringTypeSuffix));

    sidex_Variant_New_None            := Tsidex_Variant_New_None(ImportDllFunction(dllhandle,'sidex_Variant_New_None'));
    sidex_Variant_New_Boolean         := Tsidex_Variant_New_Boolean(ImportDllFunction(dllhandle,'sidex_Variant_New_Boolean'));
    sidex_Variant_New_Integer         := Tsidex_Variant_New_Integer(ImportDllFunction(dllhandle,'sidex_Variant_New_Integer'));
    sidex_Variant_New_Float           := Tsidex_Variant_New_Float(ImportDllFunction(dllhandle,'sidex_Variant_New_Float'));
    sidex_Variant_New_String          := Tsidex_Variant_New_String(ImportDllFunction(dllhandle,'sidex_Variant_New_String'+sidexStringTypeSuffix));
    sidex_Variant_New_Binary          := Tsidex_Variant_New_Binary(ImportDllFunction(dllhandle,'sidex_Variant_New_Binary'));
    sidex_Variant_New_DateTime        := Tsidex_Variant_New_DateTime(ImportDllFunction(dllhandle,'sidex_Variant_New_DateTime'+sidexStringTypeSuffix));
    sidex_Variant_New_List            := Tsidex_Variant_New_List(ImportDllFunction(dllhandle,'sidex_Variant_New_List'));
    sidex_Variant_New_Dict            := Tsidex_Variant_New_Dict(ImportDllFunction(dllhandle,'sidex_Variant_New_Dict'));
    sidex_Variant_New_DictBySize      := Tsidex_Variant_New_DictBySize(ImportDllFunction(dllhandle,'sidex_Variant_New_DictBySize'));
    sidex_Variant_New_Table           := Tsidex_Variant_New_Table(ImportDllFunction(dllhandle,'sidex_Variant_New_Table'));

    sidex_Variant_None_Check          := Tsidex_Variant_None_Check(ImportDllFunction(dllhandle,'sidex_Variant_None_Check'));
    sidex_Variant_Boolean_Check       := Tsidex_Variant_Boolean_Check(ImportDllFunction(dllhandle,'sidex_Variant_Boolean_Check'));
    sidex_Variant_Integer_Check       := Tsidex_Variant_Integer_Check(ImportDllFunction(dllhandle,'sidex_Variant_Integer_Check'));
    sidex_Variant_Float_Check         := Tsidex_Variant_Float_Check(ImportDllFunction(dllhandle,'sidex_Variant_Float_Check'));
    sidex_Variant_String_Check        := Tsidex_Variant_String_Check(ImportDllFunction(dllhandle,'sidex_Variant_String_Check'));
    sidex_Variant_Binary_Check        := Tsidex_Variant_String_Check(ImportDllFunction(dllhandle,'sidex_Variant_Binary_Check'));
    sidex_Variant_DateTime_Check      := Tsidex_Variant_DateTime_Check(ImportDllFunction(dllhandle,'sidex_Variant_DateTime_Check'));
    sidex_Variant_List_Check          := Tsidex_Variant_List_Check(ImportDllFunction(dllhandle,'sidex_Variant_List_Check'));
    sidex_Variant_Dict_Check          := Tsidex_Variant_Dict_Check(ImportDllFunction(dllhandle,'sidex_Variant_Dict_Check'));
    sidex_Variant_Table_Check         := Tsidex_Variant_Table_Check(ImportDllFunction(dllhandle,'sidex_Variant_Table_Check'));

    sidex_Variant_As_Boolean          := Tsidex_Variant_As_Boolean(ImportDllFunction(dllhandle,'sidex_Variant_As_Boolean'));
    sidex_Variant_As_Integer          := Tsidex_Variant_As_Integer(ImportDllFunction(dllhandle,'sidex_Variant_As_Integer'));
    sidex_Variant_As_Float            := Tsidex_Variant_As_Float(ImportDllFunction(dllhandle,'sidex_Variant_As_Float'));
    sidex_Variant_As_String           := Tsidex_Variant_As_String(ImportDllFunction(dllhandle,'sidex_Variant_As_String'+sidexStringTypeSuffix));
    sidex_Variant_As_String_Length    := Tsidex_Variant_As_String_Length(ImportDllFunction(dllhandle,'sidex_Variant_As_String_Length'+sidexStringTypeSuffix));
    sidex_Variant_As_Binary           := Tsidex_Variant_As_Binary(ImportDllFunction(dllhandle,'sidex_Variant_As_Binary'));
    sidex_Variant_As_Binary_Length    := Tsidex_Variant_As_Binary_Length(ImportDllFunction(dllhandle,'sidex_Variant_As_Binary_Length'));
    sidex_Variant_As_DateTime         := Tsidex_Variant_As_DateTime(ImportDllFunction(dllhandle,'sidex_Variant_As_DateTime'+sidexStringTypeSuffix));

    sidex_Variant_List_Size           := Tsidex_Variant_List_Size(ImportDllFunction(dllhandle,'sidex_Variant_List_Size'));
    sidex_Variant_List_Clear          := Tsidex_Variant_List_Clear(ImportDllFunction(dllhandle,'sidex_Variant_List_Clear'));
    sidex_Variant_List_Append         := Tsidex_Variant_List_Append(ImportDllFunction(dllhandle,'sidex_Variant_List_Append'));
    sidex_Variant_List_Set            := Tsidex_Variant_List_Set(ImportDllFunction(dllhandle,'sidex_Variant_List_Set'));
    sidex_Variant_List_Insert         := Tsidex_Variant_List_Insert(ImportDllFunction(dllhandle,'sidex_Variant_List_Insert'));
    sidex_Variant_List_DeleteItem     := Tsidex_Variant_List_DeleteItem(ImportDllFunction(dllhandle,'sidex_Variant_List_DeleteItem'));
    sidex_Variant_List_Get            := Tsidex_Variant_List_Get(ImportDllFunction(dllhandle,'sidex_Variant_List_Get'));

    sidex_Variant_Dict_Size           := Tsidex_Variant_Dict_Size(ImportDllFunction(dllhandle,'sidex_Variant_Dict_Size'));
    sidex_Variant_Dict_Clear          := Tsidex_Variant_Dict_Clear(ImportDllFunction(dllhandle,'sidex_Variant_Dict_Clear'));
    sidex_Variant_Dict_Keys           := Tsidex_Variant_Dict_Keys(ImportDllFunction(dllhandle,'sidex_Variant_Dict_Keys'));
    sidex_Variant_Dict_HasKey         := Tsidex_Variant_Dict_HasKey(ImportDllFunction(dllhandle,'sidex_Variant_Dict_HasKey'+sidexStringTypeSuffix));
    sidex_Variant_Dict_Set            := Tsidex_Variant_Dict_Set(ImportDllFunction(dllhandle,'sidex_Variant_Dict_Set'+sidexStringTypeSuffix));
    sidex_Variant_Dict_Delete         := Tsidex_Variant_Dict_Delete(ImportDllFunction(dllhandle,'sidex_Variant_Dict_Delete'+sidexStringTypeSuffix));
    sidex_Variant_Dict_Get            := Tsidex_Variant_Dict_Get(ImportDllFunction(dllhandle,'sidex_Variant_Dict_Get'+sidexStringTypeSuffix));
    sidex_Variant_Dict_First          := Tsidex_Variant_Dict_First(ImportDllFunction(dllhandle,'sidex_Variant_Dict_First'));
    try
      sidex_Variant_Dict_Next         := Tsidex_Variant_Dict_Next(ImportDllFunction(dllhandle,'sidex_Variant_Dict_Next'+sidexStringTypeSuffix));
    except
      on E: EDLLImportError do
      begin
        sidex_Variant_Dict_Next := nil;
      end;
    end;

    sidex_Variant_Table_Rows          := Tsidex_Variant_Table_Rows(ImportDllFunction(dllhandle,'sidex_Variant_Table_Rows'));
    sidex_Variant_Table_Columns       := Tsidex_Variant_Table_Columns(ImportDllFunction(dllhandle,'sidex_Variant_Table_Columns'));
    sidex_Variant_Table_ColumnNames   := Tsidex_Variant_Table_ColumnNames(ImportDllFunction(dllhandle,'sidex_Variant_Table_ColumnNames'));
    sidex_Variant_Table_HasColumn     := Tsidex_Variant_Table_HasColumn(ImportDllFunction(dllhandle,'sidex_Variant_Table_HasColumn'+sidexStringTypeSuffix));
    sidex_Variant_Table_AddRow        := Tsidex_Variant_Table_AddRow(ImportDllFunction(dllhandle,'sidex_Variant_Table_AddRow'));
    sidex_Variant_Table_DeleteRow     := Tsidex_Variant_Table_DeleteRow(ImportDllFunction(dllhandle,'sidex_Variant_Table_DeleteRow'));
    sidex_Variant_Table_GetRow        := Tsidex_Variant_Table_GetRow(ImportDllFunction(dllhandle,'sidex_Variant_Table_GetRow'));
    sidex_Variant_Table_GetField      := Tsidex_Variant_Table_GetField(ImportDllFunction(dllhandle,'sidex_Variant_Table_GetField'+sidexStringTypeSuffix));
    sidex_Variant_Table_SetField      := Tsidex_Variant_Table_SetField(ImportDllFunction(dllhandle,'sidex_Variant_Table_SetField'+sidexStringTypeSuffix));
    sidex_Variant_Table_AddColumn     := Tsidex_Variant_Table_AddColumn(ImportDllFunction(dllhandle,'sidex_Variant_Table_AddColumn'+sidexStringTypeSuffix));
    sidex_Variant_Table_DeleteColumn  := Tsidex_Variant_Table_DeleteColumn(ImportDllFunction(dllhandle,'sidex_Variant_Table_DeleteColumn'+sidexStringTypeSuffix));
    sidex_Variant_Table_GetColumnName := Tsidex_Variant_Table_GetColumnName(ImportDllFunction(dllhandle,'sidex_Variant_Table_GetColumnName'+sidexStringTypeSuffix));
    sidex_Variant_Table_DeleteRows    := Tsidex_Variant_Table_DeleteRows(ImportDllFunction(dllhandle,'sidex_Variant_Table_DeleteRows'));

    sidex_Variant_String_GetFormat    := Tsidex_Variant_String_GetFormat(ImportDllFunction(dllhandle,'sidex_Variant_String_GetFormat'+sidexStringTypeSuffix));
    sidex_Variant_String_SetFormat    := Tsidex_Variant_String_SetFormat(ImportDllFunction(dllhandle,'sidex_Variant_String_SetFormat'+sidexStringTypeSuffix));

    Result := LoadFunctions_Info_Sidex(dllhandle, sidexUseExceptions);
  except
    on E: EDLLImportError do
    begin
      if sidexUseExceptions then raise;
    end;
  end;
end;

procedure UnloadFunctions_Sidex;
begin
  sidex_Get_Version                 := nil;
  sidex_Get_Copyright               := nil;

  sidex_Get_Version_old             := nil;
  sidex_Get_Version_Ext_old         := nil;

  sidex_Set_Password                := nil;
  sidex_Create                      := nil;

  sidex_Set_DocumentName            := nil;
  sidex_Get_DocumentName            := nil;
  sidex_Clear                       := nil;

  sidex_Free                        := nil;
  sidex_Load_Content                := nil;
  sidex_Set_Content                 := nil;
  sidex_Save_Content                := nil;
  sidex_Get_Content                 := nil;
  sidex_Get_Content_Length          := nil;
  sidex_Free_Content                := nil;

  sidex_None_Write                  := nil;

  sidex_Boolean_Read                := nil;
  sidex_Boolean_Write               := nil;

  sidex_Integer_Read                := nil;
  sidex_Integer_Write               := nil;

  sidex_Float_Read                  := nil;
  sidex_Float_Write                 := nil;

  sidex_String_Length               := nil;
  sidex_String_Read                 := nil;
  sidex_String_Write                := nil;
  sidex_Free_ReadString             := nil;

  sidex_Binary_Length               := nil;
  sidex_Binary_Read                 := nil;
  sidex_Binary_Write                := nil;
  sidex_Free_Binary_ReadString      := nil;

  sidex_List_Read                   := nil;
  sidex_List_Write                  := nil;

  sidex_Dict_Read                   := nil;
  sidex_Dict_Write                  := nil;

  sidex_Table_Read                  := nil;
  sidex_Table_Write                 := nil;

  sidex_GetGroups                   := nil;
  sidex_GetKeys                     := nil;
  sidex_DeleteGroup                 := nil;
  sidex_DeleteKey                   := nil;
  sidex_HasGroup                    := nil;
  sidex_HasKey                      := nil;
  sidex_Merge                       := nil;

  // ----- Variant -----

  sidex_Variant_DecRef              := nil;
  sidex_Variant_IncRef              := nil;
  sidex_Variant_Copy                := nil;
  sidex_Variant_GetType             := nil;

  sidex_Variant_Write               := nil;
  sidex_Variant_Read                := nil;

  sidex_Variant_New_None            := nil;
  sidex_Variant_New_Boolean         := nil;
  sidex_Variant_New_Integer         := nil;
  sidex_Variant_New_Float           := nil;
  sidex_Variant_New_String          := nil;
  sidex_Variant_New_DateTime        := nil;
  sidex_Variant_New_Binary          := nil;
  sidex_Variant_New_List            := nil;
  sidex_Variant_New_Dict            := nil;
  sidex_Variant_New_DictBySize      := nil;
  sidex_Variant_New_Table           := nil;

  sidex_Variant_None_Check          := nil;
  sidex_Variant_Boolean_Check       := nil;
  sidex_Variant_Integer_Check       := nil;
  sidex_Variant_Float_Check         := nil;
  sidex_Variant_String_Check        := nil;
  sidex_Variant_Binary_Check        := nil;
  sidex_Variant_DateTime_Check      := nil;
  sidex_Variant_List_Check          := nil;
  sidex_Variant_Dict_Check          := nil;
  sidex_Variant_Table_Check         := nil;

  sidex_Variant_As_Boolean          := nil;
  sidex_Variant_As_Integer          := nil;
  sidex_Variant_As_Float            := nil;
  sidex_Variant_As_String           := nil;
  sidex_Variant_As_String_Length    := nil;
  sidex_Variant_As_DateTime         := nil;
  sidex_Variant_As_Binary           := nil;
  sidex_Variant_As_Binary_Length    := nil;

  sidex_Variant_List_Size           := nil;
  sidex_Variant_List_Clear          := nil;
  sidex_Variant_List_Append         := nil;
  sidex_Variant_List_Set            := nil;
  sidex_Variant_List_Insert         := nil;
  sidex_Variant_List_DeleteItem     := nil;
  sidex_Variant_List_Get            := nil;

  sidex_Variant_Dict_Size           := nil;
  sidex_Variant_Dict_Clear          := nil;
  sidex_Variant_Dict_Keys           := nil;
  sidex_Variant_Dict_HasKey         := nil;
  sidex_Variant_Dict_Set            := nil;
  sidex_Variant_Dict_Delete         := nil;
  sidex_Variant_Dict_Get            := nil;
  sidex_Variant_Dict_First          := nil;
  sidex_Variant_Dict_Next           := nil;

  sidex_Variant_Table_Rows          := nil;
  sidex_Variant_Table_Columns       := nil;
  sidex_Variant_Table_ColumnNames   := nil;
  sidex_Variant_Table_HasColumn     := nil;
  sidex_Variant_Table_AddRow        := nil;
  sidex_Variant_Table_DeleteRow     := nil;
  sidex_Variant_Table_GetRow        := nil;
  sidex_Variant_Table_GetField      := nil;
  sidex_Variant_Table_SetField      := nil;
  sidex_Variant_Table_AddColumn     := nil;
  sidex_Variant_Table_DeleteColumn  := nil;
  sidex_Variant_Table_GetColumnName := nil;
  sidex_Variant_Table_DeleteRows    := nil;

  sidex_Variant_String_GetFormat    := nil;
  sidex_Variant_String_SetFormat    := nil;
end;

function LoadSidex: Boolean;
begin
  if not sidexLoaded then
  begin
    if LoadDll_Sidex(sidexdllname) then
    begin
      if sidexDesigning then
      begin
        LoadFunctions_Info_Sidex(sidex_dllHandle, false);
      end
      else
      begin
        sidexLoaded := LoadFunctions_Sidex(sidex_dllHandle);
      end;
    end;
  end;
  Result := sidexLoaded;
end;

function UnloadSidex: Boolean;
begin
  UnloadFunctions_Sidex;
  Result := UnloadDll_Sidex();
  sidexLoaded := false;
end;

//----------------------------------------------
// show a message and terminate the application
//----------------------------------------------
procedure InitError(amsg : string);
begin
  {$if defined(CONSOLE)}
    try
      WriteLn('SIDEX: library error: ' + sidexdllname);
      WriteLn(amsg);
    except
      // ignore
    end;
    Sleep(3000);
  {$else}
    MessageDlg('SIDEX: library error:' + sidexdllname + #13#13 + amsg,
               {$if defined(FPC)}
                 mtError, [mbOK], 0);
               {$else}
                 {$if CompilerVersion >= 21}
                    TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
                 {$else}
                    mtError, [mbOK], 0);
                 {$ifend}
               {$ifend}
  {$ifend}
  Halt(1); // terminate application
end;

//------------------------------------------------------------------------------

function sidex_loaded : Boolean;
begin
  Result := sidexLoaded;
end;

function sidex_Copyright : string;
var
  pVal: PSIDEX_STRING;
  iLen: SIDEX_INT32;
begin
  Result := '';
  if Assigned(sidex_Get_Copyright) then
  begin
    pVal := nil;
    sidex_Get_Copyright(pVal, iLen);
    if Assigned(pVal) then Result := pVal;
  end;
end;

function sidex_Version : string;
var
  v1, v2, v3: SIDEX_INT32;
  psLib: PSIDEX_STRING;
begin
  Result := '';
  v1 := 0;
  v2 := 0;
  v3 := 0;
  if Assigned(sidex_Get_Version) then
  begin
    psLib := nil;
    sidex_Get_Version(v1, v2, psLib);
    if Assigned(psLib) then Result := psLib;
  end
  else if Assigned(sidex_Get_Version_Ext_old) then
  begin
    sidex_Get_Version_Ext_old(v1, v2, v3);
    Result := Format('%d.%d.%d', [v1, v2, v3]);
  end
  else if Assigned(sidex_Get_Version_old) then
  begin
    sidex_Get_Version_old(v1, v2);
    Result := Format('%d.%d.0', [v1, v2]);
  end;
end;

//------------------------------------------------------------------------------

initialization

sidex_dllHandle      := SIDEX_DLLHANDLE_NULL;
sidexLoaded          := false;
sidexUseExceptions   := true;
sidexDesigning       := false;
sidexDesignCopyright := '';
sidexDesignVersion   := '';

try
  {$if not defined(ANDROID)}
    if Assigned(Application) then
    begin
      sidexExeFileName := ExtractFileName(Application.ExeName);
      for sidexLoopIdx := Low(sidexExeDesigners) to High(sidexExeDesigners) do
      begin
        if AnsiCompareText(sidexExeFileName, sidexExeDesigners[sidexLoopIdx]) = 0 then
        begin
          sidexDesigning := true;
          Break;
        end;
      end;
    end;
  {$ifend}
  LoadSidex();
  if sidexDesigning then UnloadSidex();
except
  on E: EDLLImportError do
  begin
    InitError(E.Message);
  end;
  on E: Exception do
  begin
    InitError(E.Message);
  end;
  else
  begin
    InitError('Unknown error!');
  end;
end;

//------------------------------------------------------------------------------

finalization

UnloadSidex();

//------------------------------------------------------------------------------

end.


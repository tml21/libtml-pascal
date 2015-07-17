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

unit uTMLTypes;

//------------------------------------------------------------------------------

interface

uses
  uSidexTypes;

//------------------------------------------------------------------------------

type
  TML_HANDLE_TYPE     = SIDEX_HANDLE_TYPE;

  TML_CORE_HANDLE     = TML_HANDLE_TYPE;
  PTML_CORE_HANDLE    = ^TML_CORE_HANDLE;

  TML_COMMAND_HANDLE  = TML_HANDLE_TYPE;
  PTML_COMMAND_HANDLE = ^TML_COMMAND_HANDLE;

  TML_BOOL            = SIDEX_BOOL;
  PTML_BOOL           = ^TML_BOOL;

  TML_INT32           = SIDEX_INT32;
  PTML_INT32          = ^TML_INT32;

  TML_UINT32          = SIDEX_UINT32;
  PTML_UINT32         = ^TML_UINT32;

  TML_INT64           = SIDEX_INT64;
  PTML_INT64          = ^TML_INT64;

  TML_UINT64          = SIDEX_UINT64;
  PTML_UINT64         = ^TML_UINT64;

  TML_DOUBLE          = SIDEX_DOUBLE;
  PTML_DOUBLE         = ^TML_DOUBLE;

  TML_POINTER         = SIDEX_POINTER;
  PTML_POINTER        = ^TML_POINTER;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TML_COMMAND_ID_TYPE = TML_UINT32;

  TML_COMMAND_ID      = TML_COMMAND_ID_TYPE;
  PTML_COMMAND_ID     = ^TML_COMMAND_ID;

  TML_STREAM_ID       = TML_INT64;
  PTML_STREAM_ID      = ^TML_STREAM_ID;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TML_SEEK_ORIGIN     = TML_INT32;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TML_COMMAND_HANDLE_ARRAY  = Array[0..0] of TML_COMMAND_HANDLE;
  PTML_COMMAND_HANDLE_ARRAY = ^TML_COMMAND_HANDLE_ARRAY;

//------------------------------------------------------------------------------

const
  TML_CORE_HANDLE_NULL      = SIDEX_HANDLE_NULL;
  TML_COMMAND_HANDLE_NULL   = SIDEX_HANDLE_NULL;
  TML_POINTER_NULL          = SIDEX_POINTER_NULL;
  TML_STREAM_ID_NULL        = 0;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TML_FALSE                 = SIDEX_FALSE;
  TML_TRUE                  = SIDEX_TRUE;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TML_SEEK_ORIGIN_BEGINNING = 0;
  TML_SEEK_ORIGIN_CURRENT   = 1;
  TML_SEEK_ORIGIN_END       = 2;

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

end.


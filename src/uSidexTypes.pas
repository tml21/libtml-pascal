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

unit uSidexTypes;

//------------------------------------------------------------------------------

interface

//------------------------------------------------------------------------------

type

  TSIDEXStringArray = Array of string;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - -

  SIDEX_HANDLE_TYPE = Int64;

  SIDEX_HANDLE      = SIDEX_HANDLE_TYPE;
  PSIDEX_HANDLE     = ^SIDEX_HANDLE;

  SIDEX_VARIANT     = SIDEX_HANDLE_TYPE;
  PSIDEX_VARIANT    = ^SIDEX_VARIANT;

  SIDEX_BOOL        = LongInt;        // 32bit (legal values are 0 or 1)
  PSIDEX_BOOL       = ^SIDEX_BOOL;

  SIDEX_INT32       = LongInt;        // 32bit signed
  PSIDEX_INT32      = ^SIDEX_INT32;

  SIDEX_UINT32      = LongWord;       // 32bit unsigned
  PSIDEX_UINT32     = ^SIDEX_UINT32;

  SIDEX_INT64       = Int64;          // 64bit signed
  PSIDEX_INT64      = ^SIDEX_INT64;

  SIDEX_UINT64      = UInt64;         // 64bit unsigned
  PSIDEX_UINT64     = ^SIDEX_UINT64;

  SIDEX_DOUBLE      = Double;
  PSIDEX_DOUBLE     = ^SIDEX_DOUBLE;

  SIDEX_POINTER     = Pointer;
  PSIDEX_POINTER    = ^SIDEX_POINTER;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - -

  SIDEX_ENCODE      = SIDEX_INT32;
  SIDEX_DATA_TYPE   = SIDEX_INT32;

//------------------------------------------------------------------------------

const
  SIDEX_HANDLE_NULL             = 0;
  SIDEX_VARIANT_NULL            = 0;
  SIDEX_POINTER_NULL            = nil;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - -

  SIDEX_FALSE                   = 0;
  SIDEX_TRUE                    = 1;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - -

  SIDEX_ENCODE_NONE             = 0;
  SIDEX_ENCODE_CDATA            = 1;
  SIDEX_ENCODE_BASE64           = 2;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - -

  SIDEX_DATA_TYPE_UNKNOWN       =  0;
  SIDEX_DATA_TYPE_NONE          =  1;
  SIDEX_DATA_TYPE_BOOLEAN       =  2;
  SIDEX_DATA_TYPE_INTEGER       =  3;
  SIDEX_DATA_TYPE_FLOAT         =  4;
  SIDEX_DATA_TYPE_STRING        =  5;
  SIDEX_DATA_TYPE_LIST          =  6;
  SIDEX_DATA_TYPE_DICT          =  7;
  SIDEX_DATA_TYPE_TABLE         =  8;
  SIDEX_DATA_TYPE_DATETIME      =  9;
  SIDEX_DATA_TYPE_BINARY        = 10;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - -

  SIDEX_STRING_FORMAT_UNKNOWN   = 'unknown';
  SIDEX_STRING_FORMAT_SIDEX     = 'sidex';
  SIDEX_STRING_FORMAT_XML       = 'xml';
  SIDEX_STRING_FORMAT_JSON      = 'json';
  SIDEX_STRING_FORMAT_CVS       = 'cvs';
  SIDEX_STRING_FORMAT_INI       = 'ini';

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

end.


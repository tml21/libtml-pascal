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

unit uSidexErrors;

//------------------------------------------------------------------------------

interface

//------------------------------------------------------------------------------

const

DEFAULT_ERROR_MSG = 'Please read sidexErrors.h';

{
* @brief    Operation succeeded
}
SIDEX_SUCCESS = 0;

//////////////////////////////////////////////////////////////////////
// SIDEX- specific errors:

{
* @brief    Error on dump a string from or to a handle
}
SIDEX_ERR_DUMPCONTENT                 = 1001;

{
* @brief    Error signals a key holds no content
}
SIDEX_ERR_NOCONTENT                   = 1002;

{
* @brief    Type mismatch between supposed type and declared type of a key
}
SIDEX_ERR_WRONG_TYPE                  = 1003;

{
* @brief    Requested Key parameter contains blanks and or tabs (Syntax Error)
}
SIDEX_ERR_WRONG_KEY                   = 1004;

{
* @brief    Requested Group parameter contains blanks and or tabs (Syntax Error)
}
SIDEX_ERR_WRONG_GROUP                 = 1005;

{
* @brief    Mandatory key parameter is NULL
}
SIDEX_ERR_MISSING_KEY                 = 1006;

{
* @brief    A given parameter is wrong for the operation
}
SIDEX_ERR_WRONG_PARAMETER             = 1007;

{
* @brief    Problem found during dict read operation / number of dictionary nodes is not a multiple of 2
}
SIDEX_ERR_WRONG_DICT_SYNTAX           = 1008;

{
* @brief    Maximum of entries reached
}
SIDEX_ERR_DICT_FULL                   = 1009;

{
* @brief    Wrong bool text representation
}
SIDEX_ERR_INVALID_BOOLEAN             = 1010;

{
* @brief    Range overflow for integer
}
SIDEX_ERR_INVALID_INTEGER             = 1011;

{
* @brief    Range overflow for float
}
SIDEX_ERR_INVALID_FLOAT               = 1012;

{
* @brief    Unspecific Error.
}
SIDEX_ERR_COMMON                      = 1013;

{
* @brief    A table column already exists
}
SIDEX_ERR_TABLE_COLUMN_ALREADY_EXISTS = 1014;

{
* @brief    DateTime format error
}
SIDEX_ERR_INVALID_DATETIME            = 1015;

{
* @brief    Product is not licensed, so it works with restricted range
}
SIDEX_ERR_LICENSE                     = 1016;

{
* @brief    First / next is not initialized
}
SIDEX_ERR_FIRST_NEXT                  = 1017;

{
* @brief    Error in Unicode string conversion
}
SIDEX_ERR_UNICODE                     = 1018;

{
* @brief    Mandatory group parameter is NULL
}
SIDEX_ERR_MISSING_GROUP               = 1019;

{
* @brief    Requested document name contains forbidden XML tag characters (Syntax Error)
}
SIDEX_ERR_WRONG_DOC                   = 1020;

{
* @brief    Requested string encoding format is NULL or unexpected
}
SIDEX_ERR_WRONG_ENCODING_FORMAT       = 1021;

{
* @brief    Product update cycle has expired
}
SIDEX_ERR_LICENSE_UDS                 = 1022;

{
* @brief    Product license has expired
}
SIDEX_ERR_LICENSE_EXP                 = 1023;

{
* @brief    Product is not licensed for this platform
}
SIDEX_ERR_LICENSE_SYS                 = 1024;

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

end.


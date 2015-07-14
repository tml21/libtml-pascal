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

unit uTMLErrors;

//------------------------------------------------------------------------------

interface

uses
  uSidexErrors;

//------------------------------------------------------------------------------

const

DEFAULT_ERROR_MSG = 'Please read tmlErrors.h';

{
* @brief    Operation succeeded
}
TML_SUCCESS                             = SIDEX_SUCCESS;

{
* @brief    Product is not licensed, so it works with restricted range
}
TML_ERR_LICENSE                         = SIDEX_ERR_LICENSE;

{
* @brief    Product update cycle has expired / It's a must to have the same value than used in SIDEX to be consistent !
}
TML_ERR_LICENSE_UDS                     = SIDEX_ERR_LICENSE_UDS;

{
* @brief    Product license has expired / It's a must to have the same value than used in SIDEX to be consistent !
}
TML_ERR_LICENSE_EXP                     = SIDEX_ERR_LICENSE_EXP;

{
* @brief    Product is not licensed for this platform / It's a must to have the same value than used in SIDEX to be consistent !
}
TML_ERR_LICENSE_SYS                     = SIDEX_ERR_LICENSE_SYS;

{
* @brief    Error in unicode string conversion / It's a must to have the same value than used in SIDEX to be consistent !
}
TML_ERR_UNICODE                         = SIDEX_ERR_UNICODE;

//////////////////////////////////////////////////////////////////////
// TML- specific errors:

{
* @brief    I've got an error message in the tml message return content
}
TML_ERR_ERROR_MSG_RECEIVED              = 1;

{
* @brief    Shall start event handling thread while destructor is pending
}
TML_ERR_EVENT_HANDLER_IN_DESTRUCTION    = 2;

{
* @brief    A stream does not exist anymore
}
TML_ERR_STREAM_DOES_NOT_EXIST           = 11;

{
* @brief    Unable to acquire / release critical section
}
TML_ERR_CRITICAL_SECTION                = 12;

{
* @brief    Common initialization error.
}
TML_ERR_COMMON                          = 13;

{
* @brief    Connection failed.
}
TML_ERR_CONNECT                         = 14;

{
* @brief    Missing Object / possible missing connection
}
TML_ERR_MISSING_OBJ                     = 15;

{
* @brief    Missing Information
}
TML_ERR_INFORMATION_UNDEFINED           = 16;

{
* @brief    Unknown command execution state
}
TML_ERR_COMMAND_STATE_UNDEFINED         = 17;

{
* @brief    Unknown command execution mode
}
TML_ERR_COMMAND_MODE_UNDEFINED          = 18;

{
* @brief    Unknown command execution mode
}
TML_ERR_COMMAND_REPLY_TYPE_UNDEFINED    = 19;

{
* @brief    Progress value is out of range
}
TML_ERR_COMMAND_PROGRESS_RANGE          = 20;

{
* @brief    Dispatcher not created or initialized
}
TML_ERR_DISPATCHER_NOT_CREATED          = 21;

{
* @brief    A valid TMLDispatcher already exists
}
TML_ERR_DISPATCHER_ALREADY_EXISTS       = 22;

{
* @brief    The command is not registered at the dispatcher
}
TML_ERR_DISPATCHER_CMD_NOT_REGISTERED   = 23;

{
* @brief    The TMLListener is / was not initialized
}
TML_ERR_LISTENER_NOT_INITIALIZED        = 25;

{
* @brief    The TMLListener is already started
}
TML_ERR_LISTENER_ALREADY_STARTED        = 27;

{
* @brief    Communication error
}
TML_ERR_LISTENER_COMMUNICATION          = 28;

{
* @brief    The TMLSender is / was not initialized
}
TML_ERR_SENDER_NOT_INITIALIZED          = 30;

{
* @brief    The TMLSender can't register profile
}
TML_ERR_SENDER_PROFILE_REGISTRATION     = 31;

{
* @brief    The TMLSender is / Host and/or port is invalid
}
TML_ERR_SENDER_INVALID_PARAMS           = 32;

{
* @brief    The TMLSender could not initialize the channel
}
TML_ERR_CHANNEL_NOT_INITIALIZED         = 33;

{
* @brief    Communication error
}
TML_ERR_SENDER_COMMUNICATION            = 35;

{
* @brief    The Profile is not supported by the listener
}
TML_ERR_SENDER_PROFILE_NOT_SUPPORTED    = 37;

{
* @brief    Hash Table error
}
TML_ERR_HASH                            = 38;

{
* @brief    Key is not of the right type
}
TML_ERR_HASH_WRONG_TYPE                 = 39;

{
* @brief    Requested attribute is not set
}
TML_ERR_ATTRIBUTE_NOT_SET               = 40;

{
* @brief    Operation not possible if the listener is in enabled state
}
TML_ERR_NOT_OPERABLE_AT_THE_MOMENT      = 41;

{
* @brief    Communication layer / Cannot read TML state
}
TML_ERR_COM_LAYER_READ_STATE            = 42;

{
* @brief    Communication layer / Vortex frame type is error
}
TML_ERR_COM_LAYER_FRAME_TYPE_IS_ERR     = 43;

{
* @brief    Communication layer / Unexpected Vortex frame type
}
TML_ERR_COM_LAYER_FRAME_UNEXP_TYPE      = 44;

{
* @brief    Communication layer / The Server has closed the connection
}
TML_ERR_COM_LAYER_CONNECTION_CLOSE      = 45;

{
* @brief    Common initialization problem
}
TML_ERR_INITIALIZATION                  = 46;

{
* @brief    Timeout during command operation
}
TML_ERR_TIMEOUT                         = 47;

{
* @brief    If no profiles are registered
}
TML_ERR_NOPROFILES                      = 48;

{
* @brief    No more valid system resources
}
TML_ERR_SYSTEMRESOURCES                 = 49;

{
* @brief    A timeout happened on waiting for async command execution
}
TML_ERR_TIMEOUT_ON_WAIT_FOR_ASYNC       = 50;

{
* @brief    The requested object not found
}
TML_ERR_DESTINATION_OBJ_NOT_FOUND       = 51;

{
* @brief    Error on load balance calculation
}
TML_ERR_LOAD_BALANCE_CALCULATION        = 52;

{
* @brief    Unable to bind listener address Maybe the IP/port is already in use or insufficient permission
}
TML_ERR_LISTENER_ADDRESS_BINDING        = 53;

{
* @brief    System is in shutdown process
}
TML_ERR_SHUTDOWN                        = 54;

{
* @brief    Callback method not initialized / value is NULL
}
TML_ERR_STREAM_INVALID_CALLBACK         = 55;

{
* @brief    A stream address parameter is invalid
}
TML_ERR_STREAM_INVALID_ADDRESS          = 56;

{
* @brief    The stream identification is invalid
}
TML_ERR_STREAM_INVALID_IDENTIFIER       = 57;

{
* @brief    The stream size is not available
}
TML_ERR_STREAM_SIZE_NOT_AVAILABLE       = 58;

{
* @brief    The stream read operation failed
}
TML_ERR_STREAM_READ_FAILED              = 59;

{
* @brief    The stream position is not available
}
TML_ERR_STREAM_POSITION_NOT_AVAILABLE   = 60;

{
* @brief    The stream seek operation is not operable
}
TML_ERR_STREAM_SEEK_NOT_OPERABLE        = 61;

{
* @brief    The stream readBuffer operation reached end of file
}
TML_ERR_STREAM_READBUFFER_EOF           = 62;

{
* @brief    Error on stream download operation
}
TML_ERR_STREAM_DOWNLOAD_FAILED          = 63;

{
* @brief    Callback returned cancel of stream download operation
}
TML_ERR_STREAM_DOWNLOAD_CANCELED        = 64;

{
* @brief    Error on stream download during write callback
}
TML_ERR_STREAM_DOWNLOAD_WRITE_FAILED    = 65;

{
* @brief    Error / the stream is already in use - can't be opened for a second time
}
TML_ERR_STREAM_ALREADY_IN_USE           = 66;

{
* @brief    Retain open problem / profile & host & port don't match with the ID
}
TML_ERR_STREAM_OPEN_ADDRESS_MISMATCH    = 67;

{
* @brief    The stream write operation failed
}
TML_ERR_STREAM_WRITE_FAILED             = 68;

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

end.


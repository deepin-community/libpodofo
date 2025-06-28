/***************************************************************************
 *   Copyright (C) 2006 by Dominik Seichter                                *
 *   domseichter@web.de                                                    *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Library General Public License as       *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU Library General Public     *
 *   License along with this program; if not, write to the                 *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 *                                                                         *
 *   In addition, as a special exception, the copyright holders give       *
 *   permission to link the code of portions of this program with the      *
 *   OpenSSL library under certain conditions as described in each         *
 *   individual source file, and distribute linked combinations            *
 *   including the two.                                                    *
 *   You must obey the GNU General Public License in all respects          *
 *   for all of the code used other than OpenSSL.  If you modify           *
 *   file(s) with this exception, you may extend this exception to your    *
 *   version of the file(s), but you are not obligated to do so.  If you   *
 *   do not wish to do so, delete this exception statement from your       *
 *   version.  If you delete this exception statement from all source      *
 *   files in the program, then also delete it here.                       *
 ***************************************************************************/

#include "PdfString.h"

#include "PdfEncrypt.h"
#include "PdfEncoding.h"
#include "PdfEncodingFactory.h"
#include "PdfFilter.h"
#include "PdfTokenizer.h"
#include "PdfOutputDevice.h"
#include "PdfDefinesPrivate.h"

#if defined(_AIX) || defined(__sun)
#include <alloca.h>
#elif defined(__APPLE__) || defined(__linux)
#include <cstdlib>
#elif defined(_WIN32)
#include <malloc.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#ifdef PODOFO_HAVE_UNISTRING_LIB
#include <unistr.h>
#endif /* PODOFO_HAVE_UNISTRING_LIB */

namespace PoDoFo {

namespace PdfStringNameSpace {

static char g_StrEscMap[256] = { 0 };

// Generate the escape character map at runtime
static const char* genStrEscMap()
{
    const long lAllocLen = 256;
    char* map = static_cast<char*>(g_StrEscMap);
    memset( map, 0, sizeof(char) * lAllocLen );
    map[static_cast<unsigned char>('\n')] = 'n'; // Line feed (LF)
    map[static_cast<unsigned char>('\r')] = 'r'; // Carriage return (CR)
    map[static_cast<unsigned char>('\t')] = 't'; // Horizontal tab (HT)
    map[static_cast<unsigned char>('\b')] = 'b'; // Backspace (BS)
    map[static_cast<unsigned char>('\f')] = 'f'; // Form feed (FF)
    map[static_cast<unsigned char>(')')] = ')';  
    map[static_cast<unsigned char>('(')] = '(';  
    map[static_cast<unsigned char>('\\')] = '\\';

    return map;
}

};

const char * const PdfString::m_escMap        = PdfStringNameSpace::genStrEscMap();

const PdfString PdfString::StringNull        = PdfString();
#ifdef _MSC_VER
const char  PdfString::s_pszUnicodeMarker[]  = { static_cast<char>(0xFE), static_cast<char>(0xFF) };
#else
const char  PdfString::s_pszUnicodeMarker[]  = { static_cast<char>(0xFE), static_cast<char>(0xFF) };
#endif
const char* PdfString::s_pszUnicodeMarkerHex = "FEFF"; 



PdfString::PdfString()
    : m_bHex( false ), m_bUnicode( false ), m_pEncoding( NULL )
{
}

PdfString::PdfString( const std::string& sString, const PdfEncoding * const pEncoding )
    : m_bHex( false ), m_bUnicode( false ), m_pEncoding( pEncoding )
{
    Init( sString.c_str(), sString.length() );
}

PdfString::PdfString( const char* pszString, const PdfEncoding * const pEncoding )
    : m_bHex( false ), m_bUnicode( false ), m_pEncoding( pEncoding )
{
    if( pszString )
        Init( pszString, strlen( pszString ) );
}

#if defined(_MSC_VER)  &&  _MSC_VER <= 1200    // not for MS Visual Studio 6
#else
PdfString::PdfString( const wchar_t* pszString, pdf_long lLen )
{
    setFromWchar_t(pszString, lLen);
}
#endif

void PdfString::setFromWchar_t(const wchar_t* pszString, pdf_long lLen )
{
    m_bHex = false;
    m_bUnicode = true;
    m_pEncoding = NULL;

    if( pszString )
    {
        if (lLen == -1)
        {
            lLen = wcslen( pszString );
        }
        const bool wchar_t_is_two_bytes = sizeof(wchar_t) == 2;
        if( wchar_t_is_two_bytes ) 
        {
            // We have UTF16
            lLen *= sizeof(wchar_t);
            m_buffer = PdfRefCountedBuffer( lLen + 2 );
            memcpy( m_buffer.GetBuffer(), pszString, lLen );
            m_buffer.GetBuffer()[lLen] = '\0';
            m_buffer.GetBuffer()[lLen+1] = '\0';
            
            // if the buffer is a UTF-16LE string
            // convert it to UTF-16BE
#ifdef PODOFO_IS_LITTLE_ENDIAN
            SwapBytes( m_buffer.GetBuffer(), lLen );
#endif // PODOFO_IS_LITTLE_ENDIA
        }
        else
        {
            // Try to convert to UTF8
            pdf_long   lDest = 5 * lLen; // At max 5 bytes per UTF8 char
            char*  pDest = static_cast<char*>(podofo_malloc( lDest ));
            if (!pDest)
            {
                PODOFO_RAISE_ERROR(ePdfError_OutOfMemory);
            }

            size_t cnt   = wcstombs(pDest, pszString, lDest);
            if( cnt != static_cast<size_t>(-1) )
            {
                // No error
                InitFromUtf8( reinterpret_cast<pdf_utf8*>(pDest), cnt );
                podofo_free( pDest );
            }
            else
            {
                podofo_free( pDest );
                PdfError e( ePdfError_InternalLogic, __FILE__, __LINE__ );
                e.SetErrorInformation( pszString );
                throw e;
            }
        }    
    }
}

PdfString::PdfString( const char* pszString, pdf_long lLen, bool bHex, const PdfEncoding * const pEncoding )
    : m_bHex( bHex ), m_bUnicode( false ), m_pEncoding( pEncoding )
{
    if( pszString )
        Init( pszString, lLen );
}

PdfString::PdfString( const pdf_utf8* pszStringUtf8 )
    : m_bHex( false ), m_bUnicode( true ), m_pEncoding( NULL )
{
    InitFromUtf8( pszStringUtf8, strlen( reinterpret_cast<const char*>(pszStringUtf8) ) );

    m_sUtf8 = reinterpret_cast<const char*>(pszStringUtf8);
}

PdfString::PdfString( const pdf_utf8* pszStringUtf8, pdf_long lLen )
    : m_bHex( false ), m_bUnicode( true ), m_pEncoding( NULL )
{
    InitFromUtf8( pszStringUtf8, lLen );

    m_sUtf8.assign( reinterpret_cast<const char*>(pszStringUtf8), lLen );
}

PdfString::PdfString( const pdf_utf16be* pszStringUtf16 )
    : m_bHex( false ), m_bUnicode( true ), m_pEncoding( NULL )
{
    pdf_long               lBufLen = 0;
    const pdf_utf16be* pszCnt  = pszStringUtf16;
    
    while( *pszCnt )
    {
        ++pszCnt;
        ++lBufLen;
    }

    lBufLen *= sizeof(pdf_utf16be);

    m_buffer = PdfRefCountedBuffer( lBufLen + sizeof(pdf_utf16be) );
    memcpy( m_buffer.GetBuffer(), reinterpret_cast<const char*>(pszStringUtf16), lBufLen );
    m_buffer.GetBuffer()[lBufLen] = '\0';
    m_buffer.GetBuffer()[lBufLen+1] = '\0';
}

PdfString::PdfString( const pdf_utf16be* pszStringUtf16, pdf_long lLen )
    : m_bHex( false ), m_bUnicode( true ), m_pEncoding( NULL )
{
    pdf_long               lBufLen = 0;
    const pdf_utf16be* pszCnt  = pszStringUtf16;
    
    while( lLen-- )
    {
        ++pszCnt;
        ++lBufLen;
    }

    lBufLen *= sizeof(pdf_utf16be);

    m_buffer = PdfRefCountedBuffer( lBufLen + sizeof(pdf_utf16be) );
    memcpy( m_buffer.GetBuffer(), reinterpret_cast<const char*>(pszStringUtf16), lBufLen );
    m_buffer.GetBuffer()[lBufLen] = '\0';
    m_buffer.GetBuffer()[lBufLen+1] = '\0';
}

PdfString::PdfString( const PdfString & rhs )
    : PdfDataType(), m_bHex( false ), m_bUnicode( false ), m_pEncoding( NULL )
{
    this->operator=( rhs );
}

PdfString::~PdfString()
{
}

void PdfString::SetHexData( const char* pszHex, pdf_long lLen, PdfEncrypt* pEncrypt )
{
    AssertMutable();

    if( !pszHex ) 
    {
        PODOFO_RAISE_ERROR( ePdfError_InvalidHandle );
    }

    if( lLen == -1 )
        lLen = strlen( pszHex );

    // Allocate a buffer large enough for the hex decoded data
    // and the 2 terminating zeros
    m_buffer = PdfRefCountedBuffer( lLen % 2 ? ((lLen + 1) >> 1) + 2 : (lLen >> 1) + 2 );
    m_bHex   = true;
    char* pBuffer = m_buffer.GetBuffer();
    if ( pBuffer != NULL )
    {
        char val;
        char cDecodedByte = 0;
        bool bLow = true;

        while( lLen-- ) 
        {
            if( PdfTokenizer::IsWhitespace( *pszHex ) )
            {
                ++pszHex;
                continue;
            }

            val = PdfTokenizer::GetHexValue( *pszHex );
            if( bLow ) 
            {
                cDecodedByte = (val & 0x0F);
                bLow         = false;
            }
            else
            {
                cDecodedByte = ((cDecodedByte << 4) | val);
                bLow         = true;

                *pBuffer++ = cDecodedByte;
            }

            ++pszHex;
        }

        if( !bLow ) 
        {
            // an odd number of bytes was read,
            // so the last byte is 0
            *pBuffer++ = cDecodedByte;
        }

        *pBuffer++ = '\0';
        *pBuffer++ = '\0';

        // If the allocated internal buffer is too big (e.g. because of whitespaces in the data)
        // copy to a smaller buffer so that PdfString::GetLength() will be correct
        lLen = pBuffer - m_buffer.GetBuffer();
        if( static_cast<size_t>(lLen) != m_buffer.GetSize() )
        {
            PdfRefCountedBuffer temp( lLen );
            memcpy( temp.GetBuffer(), m_buffer.GetBuffer(), lLen );
            m_buffer = temp;
        }
    }
    
    if( pEncrypt )
    {
        pdf_long outBufferLen = m_buffer.GetSize() - 2 - pEncrypt->CalculateStreamOffset();
        PdfRefCountedBuffer outBuffer(outBufferLen + 16 - (outBufferLen % 16));
        
        pEncrypt->Decrypt( reinterpret_cast<unsigned char*>(m_buffer.GetBuffer()),
                           static_cast<unsigned int>(m_buffer.GetSize()-2),
                          reinterpret_cast<unsigned char*>(outBuffer.GetBuffer()),
                          outBufferLen);
        // Add trailing pair of zeros
        outBuffer.Resize(outBufferLen + 2);
        outBuffer.GetBuffer()[outBufferLen] = '\0';
        outBuffer.GetBuffer()[outBufferLen + 1] = '\0';

        // Replace buffer with decrypted value
        m_buffer = outBuffer;
    }

    // Now check for the first two bytes, to see if we got a unicode string
    if( m_buffer.GetSize() >= 4 ) 
    {
		m_bUnicode = (m_buffer.GetBuffer()[0] == static_cast<char>(0xFE) && m_buffer.GetBuffer()[1] == static_cast<char>(0xFF));
		
		if( m_bUnicode ) 
        {
            PdfRefCountedBuffer temp( m_buffer.GetSize() - 2 );
            memcpy( temp.GetBuffer(), m_buffer.GetBuffer() + 2, m_buffer.GetSize() - 2 );
            m_buffer = temp;           
        }
    }
}

void PdfString::Write ( PdfOutputDevice* pDevice, EPdfWriteMode eWriteMode, const PdfEncrypt* pEncrypt ) const
{
    // Strings in PDF documents may contain \0 especially if they are encrypted
    // this case has to be handled!

    // Peter Petrov: 17 May 2008
    // Added check - m_buffer.GetSize()
    // Now we are not encrypting the empty strings (was access violation)!
    if( pEncrypt && m_buffer.GetSize() && IsValid() )
    {
        pdf_long nInputBufferLen = m_buffer.GetSize() - 2; // Cut off the trailing pair of zeros
        pdf_long nUnicodeMarkerOffet = sizeof( PdfString::s_pszUnicodeMarker );
        if( m_bUnicode )
            nInputBufferLen += nUnicodeMarkerOffet;
        
        char * pInputBuffer = new char[nInputBufferLen];
        
        if( m_bUnicode )
        {
            memcpy(pInputBuffer, PdfString::s_pszUnicodeMarker, nUnicodeMarkerOffet);
            memcpy(&pInputBuffer[nUnicodeMarkerOffet], m_buffer.GetBuffer(), nInputBufferLen - nUnicodeMarkerOffet);
        }
        else
            memcpy(pInputBuffer, m_buffer.GetBuffer(), nInputBufferLen);
        
        pdf_long nOutputBufferLen = pEncrypt->CalculateStreamLength(nInputBufferLen);
        
        char* pOutputBuffer = new char [nOutputBufferLen];
        
        pEncrypt->Encrypt(reinterpret_cast<const unsigned char*>(pInputBuffer), nInputBufferLen, reinterpret_cast<unsigned char*>(pOutputBuffer), nOutputBufferLen);

        PdfString str( pOutputBuffer, nOutputBufferLen, true );
        str.Write( pDevice, eWriteMode, NULL );

        delete[] pInputBuffer;
        delete[] pOutputBuffer;
		
        return;
    }

    pDevice->Print( m_bHex ? "<" : "(" );
    if( m_buffer.GetSize() && IsValid() )
    {
        char* pBuf = m_buffer.GetBuffer();
        pdf_long  lLen = m_buffer.GetSize() - 2; // Cut off the trailing pair of zeros

        if( m_bHex ) 
        {
            if( m_bUnicode )
                pDevice->Write( PdfString::s_pszUnicodeMarkerHex, 4 );

            char data[2];
            while( lLen-- )
            {
                data[0]  = (*pBuf & 0xF0) >> 4;
                data[0] += (data[0] > 9 ? 'A' - 10 : '0');
                
                data[1]  = (*pBuf & 0x0F);
                data[1] += (data[1] > 9 ? 'A' - 10 : '0');
                
                pDevice->Write( data, 2 );
                
                ++pBuf;
            }
        }
        else
        {
            if( m_bUnicode ) 
            {
                pDevice->Write( PdfString::s_pszUnicodeMarker, sizeof( PdfString::s_pszUnicodeMarker ) );
            }

            while( lLen-- ) 
            {
                const char & cEsc = m_escMap[static_cast<unsigned char>(*pBuf)];
                if( cEsc != 0 ) 
                {
                    pDevice->Write( "\\", 1 );
                    pDevice->Write( &cEsc, 1 );
                }
                else 
                {
                    pDevice->Write( &*pBuf, 1 );
                }

                ++pBuf;
            }
        }
    }

    pDevice->Print( m_bHex ? ">" : ")" );
}

const PdfString & PdfString::operator=( const PdfString & rhs )
{
    this->m_bHex      = rhs.m_bHex;
    this->m_bUnicode  = rhs.m_bUnicode;
    this->m_buffer    = rhs.m_buffer;
    this->m_sUtf8     = rhs.m_sUtf8;
    this->m_pEncoding = rhs.m_pEncoding;

    return *this;
}

bool PdfString::operator>( const PdfString & rhs ) const
{
    if ( !this->IsValid() || !rhs.IsValid() )
    {
        PdfError::LogMessage( eLogSeverity_Error, "PdfString::operator> LHS or RHS was invalid PdfString" );
        return false;
    }
    
    const PdfString & str1 = *this;
    const PdfString & str2 = rhs;

    if( m_bUnicode || rhs.m_bUnicode )
    {
#ifdef _WIN32
        std::wstring sWide_1 = str1.GetStringW();
        std::wstring sWide_2 = str2.GetStringW();

        return sWide_1 > sWide_2;
#else
        std::string sUtf8_1 = str1.GetStringUtf8();
        std::string sUtf8_2 = str2.GetStringUtf8();

        return sUtf8_1 > sUtf8_2;
#endif // _WIN32
    }

    return (strcmp( str1.GetString(), str2.GetString() ) > 0);
}

bool PdfString::operator<( const PdfString & rhs ) const
{
    if ( !this->IsValid() || !rhs.IsValid() )
    {
        PdfError::LogMessage( eLogSeverity_Error, "PdfString::operator< LHS or RHS was invalid PdfString" );
        return false;
    }
    
    const PdfString & str1 = *this;
    const PdfString & str2 = rhs;

    if( m_bUnicode || rhs.m_bUnicode )
    {
#ifdef _WIN32
        std::wstring sWide_1 = str1.GetStringW();
        std::wstring sWide_2 = str2.GetStringW();

        return sWide_1 < sWide_2;
#else
        std::string sUtf8_1 = str1.GetStringUtf8();
        std::string sUtf8_2 = str2.GetStringUtf8();

        return sUtf8_1 < sUtf8_2;
#endif // _WIN32
    }

    return (strcmp( str1.GetString(), str2.GetString() ) < 0);
}

bool PdfString::operator==( const PdfString & rhs ) const
{
    if ( !this->IsValid() && !rhs.IsValid() )
    {
        PdfError::LogMessage( eLogSeverity_Error, "PdfString::operator== LHS and RHS both invalid PdfStrings" );
        return true;
    }
    else if ( !this->IsValid() || !rhs.IsValid() )
    {
        PdfError::LogMessage( eLogSeverity_Error, "PdfString::operator== LHS or RHS was invalid PdfString" );
        return false;
    }

    PdfString str1 = *this;
    PdfString str2 = rhs;

    if( m_bUnicode || rhs.m_bUnicode )
    {
        // one or both strings are unicode:
        // make sure both are unicode so that 
        // we do not loose information
        str1 = str1.ToUnicode();
        str2 = str2.ToUnicode();
    }

    return str1.m_buffer == str2.m_buffer;
}

void PdfString::Init( const char* pszString, pdf_long lLen )
{
    if( !pszString )
    {
        PODOFO_RAISE_ERROR( ePdfError_InvalidHandle );
    }

    bool bUft16LE = false;
    // check if it is a unicode string (UTF-16BE)
    // UTF-16BE strings have to start with 0xFE 0xFF
    if( lLen >= 2 ) 
    {
        m_bUnicode = (pszString[0] == PdfString::s_pszUnicodeMarker[0] && 
                      pszString[1] == PdfString::s_pszUnicodeMarker[1]);
        
        // Check also for UTF-16LE
        if( !m_bUnicode && (pszString[0] == PdfString::s_pszUnicodeMarker[1] && 
                            pszString[1] == PdfString::s_pszUnicodeMarker[0]) )
        {
            bUft16LE = true;
        }
    }
    
    // skip the first two bytes 
    if( m_bUnicode || bUft16LE )
    {
        lLen      -= 2;
        pszString += 2;
    }

    
    m_buffer = PdfRefCountedBuffer( lLen + 2 );
    memcpy( m_buffer.GetBuffer(), pszString, lLen );
    m_buffer.GetBuffer()[lLen] = '\0';
    m_buffer.GetBuffer()[lLen+1] = '\0';

    // if the buffer is a UTF-16LE string
    // convert it to UTF-16BE
    if( bUft16LE ) 
    {
        SwapBytes( m_buffer.GetBuffer(), lLen );
    }
}

void PdfString::InitFromUtf8( const pdf_utf8* pszStringUtf8, pdf_long lLen )
{
    if( !pszStringUtf8 )
    {
        PODOFO_RAISE_ERROR( ePdfError_InvalidHandle );
    }

    pdf_long        lBufLen = (lLen << 1) + sizeof(wchar_t);
    // twice as large buffer should always be enough
    std::vector<char>	bytes(lBufLen);
#if (defined(_MSC_VER)  &&  _MSC_VER < 1700) || (defined(__BORLANDC__))	// MSC before VC11 has no data member, same as BorlandC
    pdf_utf16be *pBuffer = reinterpret_cast<pdf_utf16be *>(&bytes[0]); 
#else
    pdf_utf16be *pBuffer = reinterpret_cast<pdf_utf16be *>(bytes.data()); 
#endif

    lBufLen = PdfString::ConvertUTF8toUTF16( pszStringUtf8, lLen, pBuffer, lBufLen );

    lBufLen = lBufLen > 0 ? (lBufLen-1) << 1 : 0; // lBufLen is the number of characters, we need the number of bytes now!
    m_buffer = PdfRefCountedBuffer( lBufLen + sizeof(pdf_utf16be) );
    memcpy( m_buffer.GetBuffer(), reinterpret_cast<const char*>(pBuffer), lBufLen );
    m_buffer.GetBuffer()[lBufLen] = '\0';
    m_buffer.GetBuffer()[lBufLen+1] = '\0';
}

void PdfString::InitUtf8()
{
    if( this->IsUnicode() )
    {
        // we can convert UTF16 to UTF8
        // UTF8 is at maximum 5 * characterlength.

        pdf_long  lBufferLen = (5*this->GetUnicodeLength())+2;
        char* pBuffer    = static_cast<char*>(podofo_calloc( lBufferLen, sizeof(char) ));
        if( !pBuffer )
        {
            PODOFO_RAISE_ERROR( ePdfError_OutOfMemory );
        }

        pdf_long lUtf8 = PdfString::ConvertUTF16toUTF8( reinterpret_cast<const pdf_utf16be*>(m_buffer.GetBuffer()), 
                                                    this->GetUnicodeLength(), 
                                                    reinterpret_cast<pdf_utf8*>(pBuffer), lBufferLen, ePdfStringConversion_Lenient );
        if (lUtf8 + 1 > lBufferLen) // + 1 to account for 2 bytes termination here vs. 1 byte there
        {
            pBuffer = static_cast<char*>(podofo_realloc( pBuffer, lUtf8 + 1 ) );
            if( !pBuffer )
            {
                PODOFO_RAISE_ERROR( ePdfError_OutOfMemory );
            }
            if (lUtf8 - 1 > lBufferLen)
                lUtf8 = PdfString::ConvertUTF16toUTF8( reinterpret_cast<const pdf_utf16be*>(m_buffer.GetBuffer()),
                                                       this->GetUnicodeLength(), reinterpret_cast<pdf_utf8*>(pBuffer), lUtf8 + 1);
        }

        pBuffer[lUtf8 - 1] = '\0';
        pBuffer[lUtf8] = '\0';
        m_sUtf8 = pBuffer;
        podofo_free( pBuffer );
    }
    else
    {
        PdfString sTmp = this->ToUnicode();
        m_sUtf8 = sTmp.GetStringUtf8();
    }
}

#ifdef _WIN32
const std::wstring PdfString::GetStringW() const
{
    if ( !IsValid() )
    {
        PdfError::LogMessage( eLogSeverity_Error, "PdfString::GetStringW invalid PdfString" );
        return std::wstring();
    }
    
    if( !this->IsUnicode() )
    {
        return this->ToUnicode().GetStringW();
    }

    PdfRefCountedBuffer buffer( m_buffer.GetSize() );
    memcpy( buffer.GetBuffer(), m_buffer.GetBuffer(), m_buffer.GetSize() );
#ifdef PODOFO_IS_LITTLE_ENDIAN
    SwapBytes( buffer.GetBuffer(), buffer.GetSize() );
#endif // PODOFO_IS_LITTLE_ENDIA

    std::wstring wstr( reinterpret_cast<const wchar_t*>(buffer.GetBuffer()) );

    return wstr;
}

#endif // _WIN32

#ifdef PODOFO_PUBLIC_STRING_HEX_CODEC // never set, Decode even says REMOVE :(
PdfString PdfString::HexEncode() const
{
    if( this->IsHex() )
        return *this;
    else
    {
        pdf_long                  lLen  = (m_buffer.GetSize() - 1) << 1;
        PdfString             str;
        PdfRefCountedBuffer   buffer( lLen + 1 );
        PdfMemoryOutputStream stream( buffer.GetBuffer(), lLen );

        PODOFO_UNIQUEU_PTR<PdfFilter> pFilter( PdfFilterFactory::Create( ePdfFilter_ASCIIHexDecode ) );
        pFilter->BeginEncode( &stream );
        pFilter->EncodeBlock( m_buffer.GetBuffer(), (m_buffer.GetSize() - 1) );
        pFilter->EndEncode();

        buffer.GetBuffer()[buffer.GetSize()-1] = '\0';

        str.m_buffer   = buffer;
        str.m_bHex     = true;
        str.m_bUnicode = m_bUnicode;

        return str;
    }
} 

// TODO: REMOVE
PdfString PdfString::HexDecode() const
{
    if( !this->IsHex() )
        return *this;
    else
    {
        pdf_long                  lLen = m_buffer.GetSize() >> 1;
        PdfString             str;
        PdfRefCountedBuffer   buffer( lLen );
        PdfMemoryOutputStream stream( buffer.GetBuffer(), lLen );

        PODOFO_UNIQUEU_PTR<PdfFilter> pFilter( PdfFilterFactory::Create( ePdfFilter_ASCIIHexDecode ) );
        pFilter->BeginDecode( &stream );
        pFilter->DecodeBlock( m_buffer.GetBuffer(), m_buffer.GetSize() );
        pFilter->EndDecode();

        str.m_buffer   = buffer;
        str.m_bHex     = false;
        str.m_bUnicode = m_bUnicode;

        return str;
    }
}
#endif // PODOFO_PUBLIC_STRING_HEX_CODEC 

PdfString PdfString::ToUnicode() const
{
    if( this->IsUnicode() )
    {
        return *this;
    }
    else if ( this->IsValid() )
    {
        const PdfEncoding* const pEncoding = (m_pEncoding ? 
                                              m_pEncoding : 
                                              PdfEncodingFactory::GlobalPdfDocEncodingInstance());
        return pEncoding->ConvertToUnicode( *this, NULL );
    }
    else
    {
        // can't convert because PdfString is invalid and has no buffer, so return *this
        // which means trying to convert an invalid string returns another invalid string
        // and in the special case where *this is PdfString::StringNull then ToUnicode()
        // returns PdfString::StringNull
        PdfError::LogMessage( eLogSeverity_Error, "PdfString::ToUnicode invalid PdfString" );
        return *this;
    }
}

void PdfString::SwapBytes( char* pBuf, pdf_long lLen ) 
{
    char  cSwap;
    while( lLen > 1 )
    {
        cSwap     = *pBuf;
        *pBuf     = *(pBuf+1);
        *(++pBuf) = cSwap;
        
        ++pBuf;
        lLen -= 2;
    }
}

PdfRefCountedBuffer &PdfString::GetBuffer(void)
{
	return m_buffer;
}

#ifdef PODOFO_HAVE_UNISTRING_LIB

pdf_long PdfString::ConvertUTF8toUTF16( const pdf_utf8* pszUtf8, pdf_utf16be* pszUtf16, pdf_long lLenUtf16 )
{
    return pszUtf8 ? 
        PdfString::ConvertUTF8toUTF16( pszUtf8, strlen( reinterpret_cast<const char*>(pszUtf8) ), pszUtf16, lLenUtf16 ) : 0;
}

pdf_long PdfString::ConvertUTF8toUTF16( const pdf_utf8* pszUtf8, pdf_long lLenUtf8, 
                                    pdf_utf16be* pszUtf16, pdf_long lLenUtf16,
                                    EPdfStringConversion eConversion )
{
    const uint8_t* s = reinterpret_cast<const uint8_t*>(pszUtf8);
    uint16_t* resultBuf = reinterpret_cast<uint16_t*>(pszUtf16);
    size_t sLength = lLenUtf8;
    size_t resultBufLength = lLenUtf16*sizeof(pdf_utf16be);

    u8_to_u16 (s, sLength, resultBuf, &resultBufLength);

    pdf_long lBufferLen = PODOFO_MIN( static_cast<pdf_long>(resultBufLength + 1), lLenUtf16 );
    PdfRefCountedBuffer buffer(reinterpret_cast<char*>(pszUtf16), lBufferLen * sizeof(pdf_utf16be));
    buffer.SetTakePossesion(false);
    
#ifdef PODOFO_IS_LITTLE_ENDIAN
    SwapBytes( buffer.GetBuffer(), lBufferLen*sizeof(pdf_utf16be) );
#endif // PODOFO_IS_LITTLE_ENDIAN

    // Make sure buffer is 0 termnated
    reinterpret_cast<pdf_utf16be*>(buffer.GetBuffer())[resultBufLength] = 0;
    
    return lBufferLen;
}


pdf_long PdfString::ConvertUTF16toUTF8( const pdf_utf16be* pszUtf16, pdf_utf8* pszUtf8, pdf_long lLenUtf8 )
{
    pdf_long               lLen      = 0;
    const pdf_utf16be* pszStart = pszUtf16;

    while( *pszStart )
        ++lLen;

    return ConvertUTF16toUTF8( pszUtf16, lLen, pszUtf8, lLenUtf8 );
}

// returns used, or if not enough memory passed in, needed length incl. 1 byte termination
pdf_long PdfString::ConvertUTF16toUTF8( const pdf_utf16be* pszUtf16, pdf_long lLenUtf16, 
                                    pdf_utf8* pszUtf8, pdf_long lLenUtf8, 
                                    EPdfStringConversion eConversion  )
{
    PdfRefCountedBuffer buffer(lLenUtf16 * sizeof(pdf_utf16be));
    memcpy( buffer.GetBuffer(), pszUtf16, lLenUtf16 * sizeof(pdf_utf16be) );

#ifdef PODOFO_IS_LITTLE_ENDIAN
    SwapBytes( buffer.GetBuffer(), lLenUtf16*sizeof(pdf_utf16be) );
#endif // PODOFO_IS_LITTLE_ENDIAN
   
    const uint16_t* s = reinterpret_cast<const uint16_t*>(buffer.GetBuffer());
    uint8_t* pResultBuf = reinterpret_cast<pdf_utf8*>(pszUtf8);
    
    size_t sLength = lLenUtf16;
    size_t resultBufLength = lLenUtf8;

    uint8_t* pReturnBuf = u16_to_u8( s, sLength, pResultBuf, &resultBufLength );
    if (pReturnBuf != pResultBuf)
    {
        free(pReturnBuf); // allocated by libunistring, so don't use podofo_free()
        PdfError::LogMessage( eLogSeverity_Warning, "Output string size too little to hold it\n" );
        return resultBufLength + 1;
    }

    pdf_long lBufferLen = PODOFO_MIN( static_cast<pdf_long>(resultBufLength + 1), lLenUtf8 );

    // Make sure buffer is 0 terminated
    if ( static_cast<pdf_long>(resultBufLength + 1) <= lLenUtf8 )
        pszUtf8[resultBufLength] = 0;
    else
        return resultBufLength + 1; // means: check for this in the caller to detect non-termination
    
    return lBufferLen;
}

#endif /* PODOFO_HAVE_UNISTRING_LIB */


};

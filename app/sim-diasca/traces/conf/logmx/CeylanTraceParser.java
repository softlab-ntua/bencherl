package ceylan.parser;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.lightysoft.logmx.business.ParsedEntry;
import com.lightysoft.logmx.mgr.LogFileParser;



/**
 * Ceylan LogMX Parser able to parse a log file with multi-line support and
 * Relative Date support.
 *
 * Here is an example of log file suitable for this parser:
 * """
 * <0.31.0>|Object-17|Actor.Theme.Object.SomeObject|3168318240218
 *   |08/04/2008 04:41:24|ceylan_test@myhost.org
 *   |Execution.Topic.SpecificEvent|2|No answer from Object XYZ
 * """
 *
 * @see erlang/traces/conf/logmx/TraceSample.txt
 *
 */
public class CeylanTraceParser extends LogFileParser
{

	/** Current parsed log entry */
	private ParsedEntry entry = null;


	/** Entry date format */
	private final static SimpleDateFormat DatePattern = new SimpleDateFormat(
	  "dd/MM/yyyy HH:mm:ss" ) ;


	/** Pattern to match the beginning of a trace, like '<0.31.0>|':  */
	private final static Pattern TraceBeginPattern =
		Pattern.compile("^<\\d++\\.\\d++\\.\\d++>\\|.*$");


	/** Buffer for Entry message (improves performance for multi-lines
	 * entries)  */
	private StringBuilder entryMsgBuffer = null;


	/** Key of extra user-defined field about execution time */
	private static final String ExecutionTimeKey = "Wallclock Time" ;

	/** Key of extra user-defined field about execution time */
	private static final String EmitterLocationKey = "Emitter Location" ;

	/** Key of extra user-defined field about execution time */
	private static final String CategoryKey = "Categorization" ;

	private static final String[] ArrayOfKeys =
	  { ExecutionTimeKey, EmitterLocationKey, CategoryKey } ;

	/** User-defined fields names (here, only one) */
	private static final List<String> KeysOfUserDefinedFields =
		Arrays.asList( ArrayOfKeys ) ;



	/**
	 * Returns the name of this parser.
	 *
	 * @see com.lightysoft.logmx.mgr.LogFileParser#getParserName()
	 *
	 */
	public String getParserName()
	{
		return "Ceylan Trace Parser" ;
	}


	/**
	 * Returns the supported file type for this parser.
	 *
	 * @see com.lightysoft.logmx.mgr.LogFileParser#getSupportedFileType()
	 *
	 */
	public String getSupportedFileType()
	{
		return "Ceylan trace files" ;
	}


	/**
	 * Process the new line of text read from file.
	 *
	 * @see com.lightysoft.logmx.mgr.LogFileParser#parseLine(java.lang.String)
	 *
	 */
	protected void parseLine(String line) throws Exception
	{

		// If end of file, records last entry if necessary, and exits:
		if ( line == null )
		{
			recordPreviousEntryIfExists() ;
			return ;
		}

		Matcher matcher = TraceBeginPattern.matcher(line) ;

		if ( matcher.matches() )
		{

			// Records previous found entry if exists, then create a new one:
			prepareNewEntry();

			// We are at the beginning of a trace.

			/* '|' is the field separator: */
			String[] fields = line.split( "\\|" ) ;

			/*
			 * field #0: technical identifier (PID)  -> in entry 'Thread'
			 *
			 * field #1: name of the emitter         -> last part in entry
			 * 'Emitter' (useful for automatic hierarchy sorting in the left
			 * panel of the GUI)
			 *
			 * field #2: emitter categorization      -> first part in entry
			 * 'Emitter'
			 *
			 * field #3: Execution timestamp         -> in entry 'Timestamp'
			 * (called 'Date')
			 *
			 * field #4: Wallclock time              -> in entry
			 * 'Wallclock Time'
			 *
			 * field #5: emitter location            -> in entry
			 * 'Emitter Location'
			 *
			 * field #6: message categorization      -> in entry
			 * 'Message Categorization'
			 *
			 * field #7: priority                    -> in entry 'Level'
			 *
			 * field #8: message                     -> in entry Message
			 * Next fields (if ever '|' is in message) are added to message.
			 */

			entry.setThread(  fields[0].trim() ) ;
			entry.setEmitter( fields[2].trim() + "." + fields[1].trim() ) ;
			entry.setDate(    fields[3].trim() );
			entry.setLevel(   fields[7].trim() ) ;

			// From field #8 to all that may remain:
			String remainingFields = "" ;

			Integer remainingFieldsCount = fields.length - 8 ;

			// Puts back the '|':
			for ( Integer i = 0; i < remainingFieldsCount; i++ )
			{
				remainingFields += fields[i+8].trim() ;
				if ( i != remainingFieldsCount-1 )
					remainingFields += "|" ;
			}

			/*
			 * Inserts spaces to allow line-breaking, and end-of-line to
			 * separate additional fields from actual message:
			 *
			 */
			entryMsgBuffer.append( remainingFields ) ;

			// Relative timestamp is also the execution time here:
			entry.getUserDefinedFields().put( ExecutionTimeKey,
			  fields[4].trim() ) ;

			entry.getUserDefinedFields().put( EmitterLocationKey,
			  fields[5].trim() ) ;

			entry.getUserDefinedFields().put( CategoryKey,
			  fields[6].trim() ) ;

		}
		else if (entry != null)
		{

			// Appending this line to previous entry's text:
			entryMsgBuffer.append('\n').append(line);

		}

	}


	/**
	 * Returns the ordered list of user-defined fields to display (given by
	 * their key), for each entry.
	 *
	 * @see com.lightysoft.logmx.mgr.LogFileParser#getUserDefinedFields()
	 *
	 */
	@Override
		public List<String> getUserDefinedFields() {
		return KeysOfUserDefinedFields ;
	}


	/**
	 * Returns the relative timestamp (execution time) of given entry.
	 *
	 * @see com.lightysoft.logmx.mgr.LogFileParser,
	 * getRelativeEntryDate(com.lightysoft.logmx.business.ParsedEntry)
	 *
	 */
	public Date getRelativeEntryDate( ParsedEntry pEntry ) throws Exception
	{

		final String executionTimeString = pEntry.getUserDefinedFields().get(
		  ExecutionTimeKey ).toString() ;

		return new Date( Integer.parseInt( executionTimeString ) ) ;
	}


	/**
	 * Returns the Date object for the given entry
	 *
	 * @see com.lightysoft.logmx.mgr.LogFileParser,
	 * getAbsoluteEntryDate(com.lightysoft.logmx.business.ParsedEntry)
	 *
	 */
	public Date getAbsoluteEntryDate(ParsedEntry pEntry) throws Exception
	{

		return DatePattern.parse( pEntry.getDate() ) ;

	}


	/**
	 * Sends to LogMX the current parsed log entry.
	 *
	 * @throws Exception
	 *
	 */
	private void recordPreviousEntryIfExists() throws Exception
	{

		if (entry != null)
		{
			entry.setMessage( entryMsgBuffer.toString() );
			addEntry(entry);
		}

	}


	/**
	 * Sends to LogMX the current parsed log entry, then creates a new one.
	 *
	 * @throws Exception
	 *
	 */
	private void prepareNewEntry() throws Exception
	{

		recordPreviousEntryIfExists();
		entry = createNewEntry();
		entryMsgBuffer = new StringBuilder(80) ;

		// Creates an empty Map with only one element allocated:
		entry.setUserDefinedFields( new HashMap<String, Object>(1) );

	}

}
import ply.lex as lex
import sys 
#tokens

tokens = ( 
		'err',
		'Comment','EQUALS',
		'Operator',
		'INTEGER',
    	'FLOAT',
    	'CHARACTER',
    	'STRING',
		'IDENTIFIER','abstract','based','continue','for','new','switch','default','if','package','synchronized',
        'boolean','do','goto','private','this',
        'break','double','implements','protected','throw',
        'byte','else','import','public',
        'case','instanceof','return','transient',
        'catch','extends','int','short','try',
        'char','final','interface','static','void',
        'class','finally','long','strictfp','volatile',
        'float','native','super','while','assert','const',
        'enum','true','false','null','throws','INCREMENT','DECREMENT','LBR', 'LOGIC_OR', 'LOGIC_AND' ,
		'MULEQ' , 'DIVEQ' , 'MODEQ' , 'PLEQ' , 'MINEQ' , 'LSHFTEQ' , 'RSHFTEQ' , 'ANDEQ' , 'POWEQ' , 'OREQ','L_SHIFT' , 'R_SHIFT',
		'Dot' , 'RBR' , 'SEMICOLON' , 'LEFTCURL' , 'LEFTSQUARE', 'RIGHTCURL' , 'RIGHTSQUARE','EQUAL', 'MULTIPLY', 'MINUS' , 'PLUS',
		'COMMA', 'LESSTHAN' , 'GREATERTHAN' , 'DCOLON' , 'TRIPLEDOT' , 'ATRATE' , 'COLON'
	)

t_L_SHIFT 		= r'<<'
t_R_SHIFT		= r'>>'
t_INCREMENT       = r'\+\+'

t_DECREMENT       = r'\-\-'

t_LBR			  = r'\('

t_LOGIC_OR		  = r'\|\|' 

t_LOGIC_AND 	= r'&&' 

t_EQUALS	    = r'=='

t_EQUAL  = r'='

t_MULEQ  = r'\*='

t_DIVEQ = r'/='

t_MODEQ = r'%='

t_PLEQ = r'\+='

t_MINEQ = r'-='

t_LSHFTEQ = r'<<='

t_RSHFTEQ = r'>>='

t_ANDEQ = r'&='

t_POWEQ = r'\^='

t_OREQ = r'\|='

t_ignore  = " \t"

t_Dot = r'\.'

t_RBR = r'\)'

t_SEMICOLON = r';'

t_LEFTCURL = r'\{'

t_LEFTSQUARE = r'\['

t_RIGHTCURL = r'\}'

t_RIGHTSQUARE = r'\]'

t_MULTIPLY = r'\*'

t_MINUS = r'-'

t_PLUS = r'\+'

t_COMMA = r','

t_LESSTHAN = r'<'

t_GREATERTHAN = r'>'

t_TRIPLEDOT = r'\.\.\.'

t_ATRATE = r'@'

t_DCOLON = r'::'

t_COLON = r':'
def t_Comment(t):
    r'(?://[^\n]*|/\*[^*]*\*+(?:[^/*][^*]*\*+)*/)'
    t.lexer.skip(1)

def t_err(t):
	r'(0)[0-7]*([8-9]+[0-7]*)+ | (0x|0X) | (0b|0B) | /\*(.*[\n]*.*)*[^(\*/)]'
	print("Illegal character '%s'" % t.value)
	t.lexer.skip(1)

# t_Keyword = r'''
# 			abstract|continue|for|new|switch|
# 			assert|default|if|package|synchronized|
# 			boolean|double|goto|private|this|
# 			break|do|implements|protected|throw|
# 			byte|else|import|public|throws|
# 			case|enum|instanceof|return|transient|
# 			catch|extends|int|short|try|
# 			char|final|interface|static|void|
# 			finally|long|strictfp|volatile|
# 			const|float|native|super|while
# 			'''
		
keyword = ('abstract','based','continue','for','new','switch','default','if','package','synchronized',
        'boolean','do','goto','private','this',
        'break','double','implements','protected','throw',
        'byte','else','import','public',
        'case','instanceof','return','transient',
        'catch','extends','int','short','try',
        'char','final','interface','static','void',
        'class','finally','long','strictfp','volatile',
        'float','native','super','while','assert','const',
        'enum','true','false','null','throws')
		
t_class = r'''class
		   '''

t_Operator = r'>>>=|>>>|%|\^|\||&|/|!=|<=|>=|->|\?|~|!'



# t_Literal = r'''
# 			(([0-9]|([1-9]([0-9]|_)*[0-9]))+\.([0-9]|([1-9]([0-9]|_)*[0-9]))*((e|E)(\+|-)?([0-9]|([1-9]([0-9]|_)*[0-9]))+)?[fFdD]? |
#  	 		\.([0-9]|([1-9]([0-9]|_)*[0-9]))+((e|E)(\+|-)?([0-9]|([1-9]([0-9]|_)*[0-9]))+)?[fFdD]? |
#      		([0-9]|([1-9]([0-9]|_)*[0-9]))+((e|E)(\+|-)?([0-9]|([1-9]([0-9]|_)*[0-9]))+)[fFdD]? |
# 	 		([0-9]|([1-9]([0-9]|_)*[0-9]))+((e|E)(\+|-)?([0-9]|([1-9]([0-9]|_)*[0-9]))+)?[fFdD] |
#      		(0(x|X)([0-9a-fA-F]|([0-9a-fA-F]([0-9a-fA-F]|_)*[0-9a-fA-F]))+\.?|0(x|X)([0-9a-fA-F]|([0-9a-fA-F]([0-9a-fA-F]|_)*[0-9a-fA-F]))*\.([0-9a-fA-F]|([0-9a-fA-F]([0-9a-fA-F]|_)*[0-9a-fA-F]))+)(p|P)(\+|-)?([0-9]|([1-9]([0-9]|_)*[0-9]))+[fFdD]?) |
#      		#integer
#      		(0x|0X)(([0-9a-fA-F]([0-9a-fA-F]|_)*[0-9a-fA-F])|[0-9a-fA-F])[l|L]? |
#      		(0b|0B)([0-1]([0-1]|_)*[0-1]|[0-1])[l|L] |
#      		((0)((_|[0-7])*[0-7])+[l|L]? |
# 			([1-9]([0-9]|_)*[0-9]|[0-9])[l|L]?) |
# 			#Bool and null
# 			((true) | (false) | (null)) |
# 			#string
# 			(\"([^\\\n]|(\\.))*?\") | 
# 			#char
# 			((')([^\n\r'\\] | \\b | \\f | \\t | \\n | \\r | \\' | \\\" | \\\\ | \\u[0-9a-fA-F]{4} | \\[0-7]{1,2} | \\[0-3][0-7]{2})('))
# 			'''

t_INTEGER = r'''(0x|0X)(([0-9a-fA-F]([0-9a-fA-F]|_)*[0-9a-fA-F])|[0-9a-fA-F])[l|L]? |
     		(0b|0B)([0-1]([0-1]|_)*[0-1]|[0-1])[l|L] |
     		((0)((_|[0-7])*[0-7])+[l|L]? |
			([1-9]([0-9]|_)*[0-9]|[0-9])[l|L]?)
			'''

t_FLOAT = r'''
		  (([0-9]|([1-9]([0-9]|_)*[0-9]))+\.([0-9]|([1-9]([0-9]|_)*[0-9]))*((e|E)(\+|-)?([0-9]|([1-9]([0-9]|_)*[0-9]))+)?[fFdD]? |
 	 		\.([0-9]|([1-9]([0-9]|_)*[0-9]))+((e|E)(\+|-)?([0-9]|([1-9]([0-9]|_)*[0-9]))+)?[fFdD]? |
     		([0-9]|([1-9]([0-9]|_)*[0-9]))+((e|E)(\+|-)?([0-9]|([1-9]([0-9]|_)*[0-9]))+)[fFdD]? |
	 		([0-9]|([1-9]([0-9]|_)*[0-9]))+((e|E)(\+|-)?([0-9]|([1-9]([0-9]|_)*[0-9]))+)?[fFdD] |
     		(0(x|X)([0-9a-fA-F]|([0-9a-fA-F]([0-9a-fA-F]|_)*[0-9a-fA-F]))+\.?|0(x|X)([0-9a-fA-F]|([0-9a-fA-F]([0-9a-fA-F]|_)*[0-9a-fA-F]))*\.([0-9a-fA-F]|([0-9a-fA-F]([0-9a-fA-F]|_)*[0-9a-fA-F]))+)(p|P)(\+|-)?([0-9]|([1-9]([0-9]|_)*[0-9]))+[fFdD]?)
		  '''

t_STRING = r'''(\"([^\\\n]|(\\.))*?\")
			'''

t_CHARACTER = r''' ((')([^\n\r'\\] | \\b | \\f | \\t | \\n | \\r | \\' | \\\" | \\\\ | \\u[0-9a-fA-F]{4} | \\[0-7]{1,2} | \\[0-3][0-7]{2})('))
		 '''

# t_IDENTIFIER = r'[a-zA-Z\$_][a-zA-Z\$_0-9]*'
def t_IDENTIFIER(t):
    r'[a-zA-Z\$_][a-zA-Z\$_0-9]*'
    if t.value in keyword:
        t.type = t.value.lower()
    return t

precedence = (
	('left','err'),
	('left','Identifier'),
	('left','Literal'),
	('left','Operator'),
	('left','Seperator'),
	('left','Keyword'))

 #Tracking line number
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
#  # Error handling rule

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


file = open(sys.argv[1], "r").readlines()
data = ' '.join(file)
 
# data = "int x = random.nextInt(high - low) + low;"
# print(data)
lexer = lex.lex()
lexer.input(data)

table = {}
while True:
	tok = lexer.token()
	if not tok:
		break
	if tok.value not in table:
		table[tok.value]= [tok.type,1]
	else:
		table[tok.value][1] = table[tok.value][1]+1


for key in table:
	print(key + ','+ str(table[key][0]) + ',' + str(table[key][1]))
import ply.lex as lex
import ply.yacc as yacc
import lexer 
import sys

#################################edit required
from graphviz import Digraph
import argparse
import pprint as pp
nodecount = 0
dot = Digraph()

def make_node(label):
	global nodecount
	dot.node(str(nodecount), label)
	nodecount += 1
	return nodecount - 1

def make_edge(node1, node2, label=''):
	dot.edge(str(node1), str(node2), label=label)
#################################
tokens = lexer.tokens
# print("=================================================")
# print(tokens)
# tokens = ( 
# 		'err',
# 		'Comment',
# 		'Keyword',
# 		'Seperator',
# 		'Operator',
# 		'Literal',
# 		'Identifier'
# 	)

#parser

start = 'CompilationUnit'

######################################
#packages

def p_CompilationUnit(p):
	'''CompilationUnit : PackageDeclaration ImportDeclarationStar TypeDeclarationStar
					  | PackageDeclaration ImportDeclarationStar 
					  | PackageDeclaration TypeDeclarationStar
					  | ImportDeclarationStar TypeDeclarationStar
					  | PackageDeclaration 
					  | ImportDeclarationStar 
					  | TypeDeclarationStar 
					  |
	'''
	p[0] = make_node('Compilation_Unit')
	if(len(p.slice)==4):
		make_edge(p[0],p[1])
	if(len(p.slice)==3 and str(p.slice[1])=="PackageDeclaration"):
		make_edge(p[0],p[1])
		for i in p[2]:
			make_edge(p[0],i)

	if(len(p.slice)==3 and str(p.slice[1])=='ImportDeclarationStar'):
		for i in p[1]:
			make_edge(p[0],i)
		for i in p[2]:
			make_edge(p[0],i)
	if(len(p.slice)==2 and str(p.slice[1])=='PackageDeclaration'):
		make_edge(p[0],p[1])
	if(len(p.slice)==2 and str(p.slice[1])!='PackageDeclaration'):
		for i in p[1]:
			make_edge(p[0],i)
		

def p_ImportDeclarationStar(p):
	'''ImportDeclarationStar : ImportDeclaration 
							  |  ImportDeclarationStar ImportDeclaration 
	'''
	if(len(p.slice)==2):
		p[0] = [p[1]]
	else:
		p[0] = [p[1]]
		p[0].append(p[2])

def p_TypeDeclarationStar(p):
	'''TypeDeclarationStar :  TypeDeclaration
							| TypeDeclarationStar TypeDeclaration
	'''
	if(len(p.slice)==2):
		p[0] = [p[1]]
	else:
		p[0] = [p[1]]
		p[0].append(p[2])

def p_PackageDeclaration(p): # identifier star { . identifier } 
	'''PackageDeclaration : package Name  SEMICOLON
	'''
	p[0] = make_node(p[1])
	make_edge(p[0],p[2])
	

def p_ImportDeclaration(p):
	'''ImportDeclaration : SingleTypeImportDeclaration 
						 | TypeImportOnDemandDeclaration
	'''
	p[0] = p[1]

def p_SingleTypeImportDeclaration(p):
	'''SingleTypeImportDeclaration : import Name SEMICOLON
	'''
	p[0] = make_node(p[1])
	make_edge(p[0],p[2])

def p_TypeImportOnDemandDeclaration(p):
	'''TypeImportOnDemandDeclaration : import Name Dot MULTIPLY SEMICOLON
	'''
	p[0] = make_node(p[1])  #doubt
	make_edge(p[0],p[2])
	temp = make_node('.')
	temp2 = make_node('*')
	make_edge(p[0],temp)
	make_edge(p[0],temp2)
	
def p_TypeDeclaration(p):
	'''TypeDeclaration : ClassDeclaration
						| SEMICOLON	
	'''
	if(p.slice[1]=='SEMICOLON'):
		p[0] = make_node(p[1])
	else:
		p[0] = p[1]

## Arraays 

def p_ArrayInitializer(p):
	'''ArrayInitializer : LEFTCURL  VariableInitializerList COMMA  RIGHTCURL
						| LEFTCURL  VariableInitializerList   RIGHTCURL
						| LEFTCURL   COMMA  RIGHTCURL
						| LEFTCURL  RIGHTCURL
	'''
	p[0] = make_node('ArrayInitializer')
		
	if(len(p)== 5 ):
		for i in p[2]:
			make_edge(p[0],i)
	elif(len(p) == 4):
		for i in p[2]:
			make_edge(p[0],i)
	else:
		p[0]=0
	
def p_VariableInitializerList(p):
	'''VariableInitializerList : VariableInitializer
						|  VariableInitializerList COMMA VariableInitializer 
 	'''
	if(len(p)== 2):
		p[0] = p[1]
	else:
		p[0]= p[1]
		p[0].append(p[4])

## Expressions 
def p_Primary(p):
	'''Primary : PrimaryNoNewArray
			  | ArrayCreationExpression
	'''
	p[0] = p[1]

def p_PrimaryNoNewArray(p):
	'''PrimaryNoNewArray : Literal
						| this
						| LBR Expression RBR
						| ClassInstanceCreationExpression
						| FieldAccess
						| ArrayAccess
						| MethodInvocation
	'''
	if(len(p.slice)==4):
		p[0] = p[2]
	elif(str(p.slice[1])=="this"):
		p[0] = make_node("this")
	else:
		p[0] = p[1]

def p_ClassInstanceCreationExpression(p):
	'''
    ClassInstanceCreationExpression : new ClassType LBR RBR
    						| new ClassType LBR ArgumentList RBR
    '''
	if(p[1]=='Literal'):
		p[0]=make_node(str(p[1]))
	elif(p[1]=='this'):
		p[0]=make_node(str(p[1]))
	elif (len(p)== 4):
		p[0] = p[2]
	else:
		p[0] = p[1]

def p_ClassType(p):
    '''
    ClassType : Name
    '''
    p[0] = p[1]

def p_FieldAccess(p):
	'''FieldAccess : Primary Dot Identifier
				   | super Dot Identifier
	'''
	p[0] = make_node(p[1])
	make_edge(p[0], p[3])

def p_ArrayAccess(p):
	''' ArrayAccess : Name LEFTSQUARE Expression RIGHTSQUARE
					| PrimaryNoNewArray LEFTSQUARE Expression RIGHTSQUARE
	'''
	p[0] = make_node('arrayAccess')
	make_edge(p[0], p[1])
	make_edge(p[0], p[3])

def p_MethodInvocation(p):
	''' MethodInvocation : Name LBR  ArgumentList RBR 
						| Name LBR   RBR
						| Primary Dot  Identifier LBR ArgumentList RBR
						| Primary Dot  Identifier LBR  RBR
						| super Dot  Identifier LBR ArgumentList RBR
						| super Dot  Identifier LBR  RBR

	'''
	p[0]  = make_node('MethodInvocation')
	make_edge(p[0],p[1])
	if(len(p)==5):
		for i in p[4]:
			make_edge(p[0],i)
	elif(len(p)== 7 ):
		make_edge(p[0],p[3])
		for i in p[5]:
			make_edge(p[0],i)
	elif(len(p)== 6):
		make_edge(p[0], p[3])
	
def p_ArgumentList(p):
	''' ArgumentList :  Expression
					 | ArgumentList COMMA Expression 
	'''
	if(len(p)==2):
		p[0] = p[1]
	else:
		p[0] = p[1]
		p[0].append(p[3])

def p_ArrayCreationExpression(p):
    '''
    ArrayCreationExpression : new PrimitiveType DimExprs
    | new Name DimExprs
    '''
    p[0] = make_node('new')
    make_edge(p[0],p[2])

    for i in p[3]:
        make_edge(p[0], i)
	
def p_DimExprs(p):
	'''DimExprs : DimExpr 
			   | DimExpr DimExprs
	'''
	if(len(p)== 2):
		p[0]=[p[1]]
	else:
		p[0] = p[2]
		p[0].append(p[1])

def p_DimExpr(p):
	'''DimExpr : LEFTSQUARE Expression RIGHTSQUARE
	'''
	p[0] = p[2]

def p_Expression(p):
	'''Expression : AssignmentExpression
	'''
	p[0] = p[1]

def p_AssignmentExpression(p):
	'''AssignmentExpression : ConditionalExpression
							| Assignment
	'''
	p[0] = p[1]

def p_Assignment(p): 
	''' Assignment : LeftHandSide AssignmentOperator AssignmentExpression 
	'''
	p[0] = make_node('Assignment')
	make_edge(p[0], p[1])
	make_edge(p[0], p[3])

def p_LeftHandSide(p):
	'''LeftHandSide : Name
					| FieldAccess
					| ArrayAccess
	'''	
	p[0] = p[1]	

def p_AssignmentOperator(p):
	'''AssignmentOperator : EQUAL
						  |  MULEQ
						  |  DIVEQ
						  |  MODEQ
						  |  PLEQ
						  |  MINEQ
						  |  LSHFTEQ
						  |  RSHFTEQ
						  |  ANDEQ
						  |  POWEQ
						  |  OREQ 
	'''
	p[0] = p[1]

def p_ConditionalExpression(p):
	'''ConditionalExpression : ConditionalOrExpression
							| ConditionalOrExpression '?' Expression COLON ConditionalExpression
	'''
	if(len(p)==2):
		p[0] = [p[1]]
	else:
		p[0] = p[5] 
		p[0].append(p[1])
		p[0].append(p[3])


def p_ConditionalOrExpression(p):
	'''ConditionalOrExpression : ConditionalAndExpression
							   | ConditionalOrExpression LOGIC_OR ConditionalAndExpression

	'''
	if(len(p)==2):
		p[0] = p[1]
	else:
		p[0] = make_node('LOGIC_OR')
		make_edge(p[0], p[1])
		make_edge(p[0], p[3])
	
def p_ConditionalAndExpression(p):
	'''ConditionalAndExpression : InclusiveOrExpression
								| ConditionalAndExpression LOGIC_AND InclusiveOrExpression
	'''
	if(len(p)==2):
		p[0] = p[1]
	else:
		p[0] = make_node('LOGIC_AND')
		make_edge(p[0], p[1])
		make_edge(p[0], p[3])

def p_InclusiveOrExpression(p):
	'''InclusiveOrExpression : ExclusiveOrExpression
							| InclusiveOrExpression '|' ExclusiveOrExpression
	'''
	if(len(p)==2):
		p[0] = p[1]
	else:
		p[0] = make_node('LOGIC_OR')
		make_edge(p[0], p[1])
		make_edge(p[0], p[3])


def p_ExclusiveOrExpression(p):
	'''ExclusiveOrExpression : AndExpression
							| ExclusiveOrExpression '^' AndExpression 
	'''
	if(len(p)==2):
		p[0] = p[1]
	else:
		p[0] = make_node('LOGIC_XOR')
		make_edge(p[0], p[1])
		make_edge(p[0], p[3])

def p_AndExpression(p):
	'''AndExpression : EqualityExpression
					| AndExpression '&' EqualityExpression
	'''
	if(len(p)==2):
		p[0] = p[1]
	else:
		p[0] = make_node('LOGIC_AND')
		make_edge(p[0], p[1])
		make_edge(p[0], p[3])


def p_EqualityExpression(p):
	'''EqualityExpression : RelationalExpression
						  | EqualityExpression EQUALS RelationalExpression
						  | EqualityExpression '!' EQUAL RelationalExpression
	'''
	if(len(p)==2):
		p[0] = p[1]
	else:
		p[0] = make_node('LOGIC_OR')
		make_edge(p[0], p[1])
		make_edge(p[0], p[3])
	

def p_RelationalExpression(p):
	'''RelationalExpression : ShiftExpression
	   						| RelationalExpression LESSTHAN ShiftExpression
							| RelationalExpression '>' ShiftExpression
							| RelationalExpression LESSTHAN EQUAL ShiftExpression
							| RelationalExpression '>' EQUAL ShiftExpression
							| RelationalExpression instanceof ReferenceType
	'''
	if(len(p)==2):
		p[0]= p[1]
	elif(len(p)==4):
		p[0] = make_node(str(p[2]))
		make_edge(p[0], p[1])
		make_edge(p[0], p[3])


def p_ShiftExpression(p):
	'''ShiftExpression : AdditiveExpression
					  | ShiftExpression L_SHIFT AdditiveExpression
					  | ShiftExpression R_SHIFT AdditiveExpression
	'''
	if(len(p)==2):
		p[0]=p[1]
	elif(p[2]=='L_SHIFT'):
		p[0] = make_node('L_SHIFT')
		make_edge(p[0], p[1])
		make_edge(p[0], p[4])
	else:
		p[0] = make_node('R_SHIFT')
		make_edge(p[0], p[1])
		make_edge(p[0], p[4])
	

def p_AdditiveExpression(p):
	'''AdditiveExpression : MultiplicativeExpression
						  | AdditiveExpression PLUS MultiplicativeExpression
						  | AdditiveExpression MINUS MultiplicativeExpression
	'''
	if(len(p)==2):
		p[0] = p[1]
	else:
		p[0] = make_node(str(p[2]))
		make_edge(p[0],p[1])
		make_edge(p[0], p[3])


def p_MultiplicativeExpression(p):
	'''MultiplicativeExpression : UnaryExpression
								| MultiplicativeExpression MULTIPLY UnaryExpression
								| MultiplicativeExpression '/' UnaryExpression
								| MultiplicativeExpression '%' UnaryExpression
	'''
	if(len(p)==2):
		p[0] = p[1]
	else:
		p[0] = make_node(str(p[2]))
		make_edge(p[0],p[1])
		make_edge(p[0], p[3])

def p_UnaryExpression(p):
	'''UnaryExpression : PreIncrementExpression
					 | PreDecrementExpression
					 | PLUS UnaryExpression
					 | MINUS UnaryExpression
					 | UnaryExpressionNotPlusMinus
	'''
	if(len(p)==2):
		p[0] = p[1]
	else:
		p[0] = p[2]
		temp = make_node(str(p[1]))
		make_edge(p[0], temp)
	
def p_PreIncrementExpression(p):
	'''PreIncrementExpression : INCREMENT UnaryExpression
	'''
	p[0] = p[2]
	temp = make_node('INCREMENT')
	make_edge(p[0], temp)

def p_PreDecrementExpression(p):
	'''PreDecrementExpression : DECREMENT UnaryExpression
	'''
	p[0] = p[2]
	temp = make_node('DECREMENT')
	make_edge(p[0], temp)

def p_UnaryExpressionNotPlusMinus(p):
	'''UnaryExpressionNotPlusMinus : PostfixExpression 
								| '~' UnaryExpression
								| '!' UnaryExpression
								| CastExpression
	'''
	if(len(p)==2):
		p[0] = p[1]
	else:
		p[0] = p[2]
		temp = make_node(p[1])
		make_edge(p[0], temp)

def p_PostfixExpression(p):
	'''PostfixExpression : Primary
						| Name
						| PostIncrementExpression
						| PostDecrementExpression
	'''
	p[0] = p[1]

def p_PostIncrementExpression(p):
	'''PostIncrementExpression : PostfixExpression INCREMENT
	'''
	p[0] = p[1]
	temp = make_node('INCREMENT')
	make_edge(p[0], temp)


def p_PostDecrementExpression(p):
	'''PostDecrementExpression : PostfixExpression DECREMENT
	'''
	p[0]=p[1]
	temp = make_node('DECREMENT')
	make_edge(p[0], temp)
	

def p_CastExpression(p): #additionalbound paren 
	'''CastExpression : LBR PrimitiveType RBR UnaryExpression
	'''
	p[0]= p[4]
	make_edge(p[0],p[2])

#Mukul ####################################################################
 # Error rule for syntax errors

def p_Identifier(p):
	'''Identifier : IDENTIFIER
	'''
	p[0] = make_node(p[1])
	
def p_Literal(p):
    ''' Literal : IntegerLiteral
	| FloatingPointLiteral
	| CharacterLiteral
	| StringLiteral
	| NullLiteral
	'''
    p[0] = p[1]

def p_IntegerLiteral(p):
    '''IntegerLiteral : INTEGER
    '''
    p[0] = make_node((p.slice[1].value))

def p_FloatingPointLiteral(p):
    '''
    FloatingPointLiteral : FLOAT
    '''
    p[0] = make_node(str(p.slice[1].value))

def p_CharacterLiteral(p):
    '''
    CharacterLiteral : CHARACTER
    '''
    p[0] = make_node(str(p.slice[1].value))

def p_StringLiteral(p):
    '''
    StringLiteral : STRING
    '''
    p[0] = make_node(str(p.slice[1].value))

def p_NullLiteral(p):
    '''
    NullLiteral : null
    '''
    p[0] = make_node(str(p.slice[1].value))

def p_BooleanLiteral(p):
	'''
	BooleanLiteral : true 
					| false
	'''
	p[0] = make_node(str(p.slice[1].value))

def p_Name(p):
	''' Name : SimpleName
	          | QualifiedName
	'''
	p[0] = p[1]

def p_SimpleName(p):
	''' SimpleName : Identifier
	'''
	p[0] = p[1]

def p_QualifiedName(p):
	''' QualifiedName : Name Dot Identifier
	'''
	p[0] = make_node(str('.'))
	make_edge(p[0],p[1])
	make_edge(p[0],p[3])


def p_ClassDeclaration(p):
	''' ClassDeclaration :   Modifiers class Identifier Superclass ClassBody
						| class Identifier Superclass ClassBody
						| Modifiers class Identifier ClassBody
						| class Identifier ClassBody
	'''
	if(str(p.slice[1])=='Modifiers'):
		p[0] = make_node(p.slice[2].value)
		if(len(p.slice)==6):
			for i in p[1]:
				make_edge(p[0],i)
			make_edge(p[0],p[3])
			make_edge(p[0],p[4])
			make_edge(p[0],p[5])
		if(len(p.slice)==5):
			for i in p[1]:
				make_edge(p[0],i)
			make_edge(p[0],p[3])
			make_edge(p[0],p[4])
	else:
		p[0] = make_node(p.slice[1].value)

def p_Modifiers(p):
	''' Modifiers : Modifier
	             | Modifiers Modifier
	'''
	if(len(p.slice)==2):
		p[0] = [p[1]]
	else:
		p[0] = p[1]
		p[0].append(p[2])

def p_Modifier(p):
	''' Modifier : public 
				| protected
				| private
				| static
				| abstract
				| final 
				| native 
				| synchronized
				| transient
				| volatile
	'''
	p[0] = make_node(p.slice[1].value)

def p_Superclass(p):
	''' Superclass : extends Name
	'''
	p[0] = make_node(p.slice[1].value)
	make_edge(p[0],p[2])

def p_ClassBody(p):
	''' ClassBody : LEFTCURL RIGHTCURL
			 | LEFTCURL ClassBodyDeclarationStar RIGHTCURL
	'''
	p[0] = make_node('ClassBody')
	if(len(p.slice)==4):
		for i in p[2]:
			make_edge(p[0],i)
	

def p_ClassBodyDeclaration(p):
	''' ClassBodyDeclaration : ClassMemberDeclaration
							   | StaticInitializer
							   | ConstructorDeclaration
	'''
	p[0] = p[1]

def p_ClassMemberDeclaration(p):
	''' ClassMemberDeclaration : FieldDeclaration
	                          | MethodDeclaration
							  | ClassDeclaration
	'''
	p[0] = p[1]

def p_FieldDeclaration(p):
	''' FieldDeclaration : Modifiers Type VariableDeclarators SEMICOLON
						| Type VariableDeclarators SEMICOLON
	'''
	if(len(p.slice)==5):
		p[0] = p[2]
		for i in p[1]:
			make_edge(p[0],i)
		for i in p[3]:
			make_edge(p[0],i)
	else:
		p[0] = p[1]
		for i in p[2]:
			make_edge(p[0],i)


def p_VariableDeclarators(p):
	''' VariableDeclarators : VariableDeclarators COMMA VariableDeclarator
	                         | VariableDeclarator
	'''
	if(len(p.slice)==2):
		p[0] = [p[1]]
	else:
		p[0] = p[1]
		p[0].append(p[3])

def p_VariableDeclarator(p):
    ''' VariableDeclarator : VariableDeclaratorId EQUAL VariableInitializer
	                      | VariableDeclaratorId
	'''
    if(len(p.slice)==2):
    	p[0] = p[1]
    else:
    	p[0] = make_node('=')
    	make_edge(p[0],p[1])
    	make_edge(p[0],p[3])

def p_VariableDeclaratorId(p):
	'''
        VariableDeclaratorId :  Identifier Dims
		                    |  Identifier
    '''
	if(len(p.slice)==2):
		p[0] = p[1]
	else:
		p[0] = p[1]
		make_edge(p[0],p[2])

def p_VariableInitializer(p):
	'''
       VariableInitializer :  Expression
                           | ArrayInitializer
    '''
	p[0] = p[1]

def p_MethodDeclaration(p):
		'''MethodDeclaration : MethodHeader MethodBody
		'''
		p[0] = make_node('method')
		make_edge(p[0],p[1])
		make_edge(p[0],p[2])

def p_MethodHeader(p):
		'''MethodHeader : Modifiers Type MethodDeclarator Throws
		            | Modifiers Type MethodDeclarator 
					| Modifiers void MethodDeclarator Throws
					| Modifiers void MethodDeclarator 
					| Type MethodDeclarator Throws
		            | Type MethodDeclarator 
					| void MethodDeclarator Throws
					| void MethodDeclarator 
		'''
		p[0] = make_node('header')
		if(len(p.slice)==5):
			for i in p[1]:
				make_edge(p[0],i)
			if(str(p.slice[2])=="void"):
				temp = make_node("void")
				make_edge(p[0],temp)
			else:
				make_edge(p[0],p[2])
			make_edge(p[0],p[3])
			make_edge(p[0],p[4])
		elif(len(p.slice)==4):
			if(str(p.slice[1])=="Modifiers"):
				for i in p[1]:
					make_edge(p[0],i)
				if(str(p.slice[2])=="void"):
					temp = make_node("void")
					make_edge(p[0],temp)
				else:
					make_edge(p[0],p[2])
				make_edge(p[0],p[3])
			else:
				if(str(p.slice[1])=="void"):
					temp = make_node("void")
					make_edge(p[0],temp)
				else:
					make_edge(p[0],p[1])
				make_edge(p[0],p[2])
				make_edge(p[0],p[3])
		else:
			if(str(p.slice[1])=="void"):
				temp = make_node("void")
				make_edge(p[0],temp)
			else:
				make_edge(p[0],p[1])
			make_edge(p[0],p[2])
			
				

				
				


def p_MethodDeclarator(p):
		'''MethodDeclarator : Identifier LBR FormalParameterList RBR
						| Identifier LBR RBR
						| MethodDeclarator LEFTSQUARE RIGHTSQUARE 
		'''
		if(str(p.slice[1])=="Identifier" and len(p.slice)==5):
			p[0] = make_node("methodeclare")
			make_edge(p[0],p[1])
			make_edge(p[0],p[3])
		elif(str(p.slice[1])=="Identifier" and len(p.slice)==4):
			p[0] = make_node("methodeclare")
			make_edge(p[0],p[1])
		else:
			p[0] = p[1]
			temp = make_node("[]")
			make_edge(p[0],temp)


def p_FormalParameterList(p):
	'''FormalParameterList : FormalParameter
						| FormalParameterList COMMA FormalParameter
	'''
	if(len(p)== 2):
		p[0] = [p[1]]
	else:
		p[0]= p[1]
		p[0].append(p[3])

def p_FormalParameter(p):
	'''FormalParameter : Modifiers Type VariableDeclaratorId
							| Type VariableDeclaratorId
	'''
	p[0] = make_node('parameter')
	if(len(p.slice)==4):
		for i in p[1]:
			make_edge(p[0],i)
		make_edge(p[0],p[2])
		make_edge(p[0],p[3])
	else:
		make_edge(p[0],p[1])
		make_edge(p[0],p[2])


def p_Throws(p):
	'''Throws : throws ClassTypeList
	'''
	p[0] = make_node('throws')
	for i in p[1]:
		make_edge(p[0],i)

def p_ClassTypeList(p):
	'''ClassTypeList :	ClassTypeList COMMA ClassType
						| 	ClassType
	'''
	if(len(p.slice)==2):
		p[0] = [p[1]]
	else:
		p[0] = p[1]
		p[0].append(p[3])

def p_MethodBody(p):
	'''MethodBody : Block
		  		| SEMICOLON
	'''
	if(str(p.slice[1])=="SEMICOLON"):
		p[0] = make_node(";")
	else:
		p[0] = p[1]
def p_StaticInitializer(p):
	'''StaticInitializer : static Block
	'''
	p[0] = make_node("static")
	make_edge(p[0],p[2])


def p_ConstructorDeclaration(p):
	'''ConstructorDeclaration : Modifiers ConstructorDeclarator Throws ConstructorBody
								| ConstructorDeclarator Throws ConstructorBody
								| ConstructorDeclarator ConstructorBody
								| Modifiers ConstructorDeclarator ConstructorBody
	'''
	if(str(p.slice[1])=="Modifier"):
		p[0] = p[2]
		for i in p[1]:
			make_edge(p[0],i)
		if(len(p.slice)==5):
			make_edge(p[0],p[3])
			make_edge(p[0],p[4])
		else:
			make_edge(p[0],p[3])
	else:
		p[0] = p[2]
		if(len(p.slice)==4):
			make_edge(p[0],p[2])
			make_edge(p[0],p[3])
		else:
			make_edge(p[0],p[2])



def p_ConstructorDeclarator(p):
	'''ConstructorDeclarator : SimpleName LBR FormalParameterList RBR
								| SimpleName LBR RBR
	'''
	p[0] = p[1]
	if(len(p.slice)==5):
		for i in p[3]:
			make_edge(p[0],i)			

def p_ConstructorBody(p):
		''' ConstructorBody : LEFTCURL ExplicitConstructorInvocation BlockStatements RIGHTCURL
    							| LEFTCURL ExplicitConstructorInvocation RIGHTCURL
    							| LEFTCURL BlockStatements RIGHTCURL
    							| LEFTCURL RIGHTCURL
		'''
		p[0] = make_node("constructor_body")
		if(len(p.slice)==5):
			make_edge(p[0],p[2])
			for i in p[3]:
				make_edge(p[0],i)
		elif(len(p.slice)==4):
			make_edge(p[0],p[2])
		elif(len(p.slice)==3):
			for i in p[2]:
				make_edge(p[0],i)


def p_ExplicitConstructorInvocation(p):
		'''	ExplicitConstructorInvocation :   this LBR ArgumentList RBR SEMICOLON
										|	 this LBR RBR SEMICOLON
										|	 super LBR ArgumentList RBR SEMICOLON
										|	super LBR RBR SEMICOLON
		'''
		if(str(p.slice[1])=="this"):
			p[0] = make_node("this")
			if(len(p.slice)==6):
				for i in p[3]:
					make_edge(p[0],i)
		else:
			p[0] = make_node("super")
			if(len(p.slice)==6):
				for i in p[3]:
					make_edge(p[0],i)



def p_ClassBodyDeclarationStar(p):
	''' ClassBodyDeclarationStar : ClassBodyDeclaration
							| ClassBodyDeclarationStar ClassBodyDeclaration
	'''
	if(len(p.slice)==2):
		p[0] = [p[1]]
	else:
		p[0] = p[1]
		p[0].append(p[2])

def p_Type(p):
	''' Type : PrimitiveType
			  | ReferenceType
	'''
	p[0] = p[1]

def p_PrimitiveType(p):
	''' PrimitiveType :  NumericType
					  | BooleanLiteral
	'''
	p[0] = p[1]

def p_NumericType(p):
	''' NumericType : IntegralType
					| FloatingPointType
	'''
	p[0] = p[1]

def p_IntergralType(p):
	'''IntegralType : byte
					| short
					| int
					| long
					| char
	'''
	p[0] = make_node(p.slice[1].value)

def p_FloatingPointType(p):
	'''FloatingPointType : float
						| double
	'''
	p[0] = make_node(p.slice[1].value)

def p_ReferenceType(p):
	'''ReferenceType : Name
					| ArrayType
	'''
	p[0] = p[1]


def p_ArrayType(p):
	'''ArrayType : PrimitiveType Dims
				| Name Dims
				| ArrayType Dims
	'''
	p[0] = p[1]
	make_edge(p[0],p[2])


def p_Dims(p):
	''' Dims : LEFTSQUARE RIGHTSQUARE
			| Dims LEFTSQUARE RIGHTSQUARE
	'''
	p[0] = make_node("[]")



#Blocks and Statements

def p_Block(p):
	'''Block : LEFTCURL RIGHTCURL
			 | LEFTCURL BlockStatements RIGHTCURL
	'''
	p[0] = make_node('Block')
	if(len(p.slice)==4):
		for i in p[2]:
			make_edge(p[0],i)

def p_BlockStatements(p):
	'''BlockStatements : BlockStatement
					   | BlockStatements BlockStatement
	'''
	if(len(p)==3):
		p[0] = p[1]
		p[0].append(p[2])
	else:
		p[0] = [p[1]]


def p_BlockStatement(p):
	'''BlockStatement : LocalVariableDeclarationStatement
					 | Statement
	'''
	p[0] = p[1]

def p_LocalVariableDeclarationStatement(p):
	'''LocalVariableDeclarationStatement : LocalVariableDeclaration SEMICOLON
	'''
	p[0] = p[1]

def p_LocalVariableDeclaration(p):
	'''LocalVariableDeclaration : Type VariableDeclarators
	'''
	p[0] = p[1]
	for i in p[2]:
		make_edge(p[0],i)

def p_Statement(p):
	'''Statement : StatementWithoutTrailingSubstatement
				| LabeledStatement
				| IfThenStatement
				| IfThenElseStatement
				| WhileStatement
				| ForStatement
	'''
	p[0] = p[1]

def p_StatementNoShortIf(p):
	'''StatementNoShortIf : StatementWithoutTrailingSubstatement
						 | LabeledStatementNoShortIf
						 | IfThenElseStatementNoShortIf
						 | WhileStatementNoShortIf
						 | ForStatementNoShortIf
	'''
	p[0] = p[1]

def p_StatementWithoutTrailingSubstatement(p):
	'''StatementWithoutTrailingSubstatement : Block
											| EmptyStatement
											| ExpressionStatement
											| BreakStatement
											| ContinueStatement
											| ReturnStatement
											| ThrowStatement
											| TryStatement
	'''
	p[0] = p[1]

def p_EmptyStatement(p):
	'''EmptyStatement : SEMICOLON
	'''
	p[0] = make_node(';')

def p_LabeledStatement(p):
	'''LabeledStatement : Identifier COLON Statement
	'''
	p[0] = make_node(':')
	make_edge(p[0],p[1])
	make_edge(p[0],p[2])

def p_LabeledStatementNoShortIf(p):
	'''LabeledStatementNoShortIf : Identifier COLON StatementNoShortIf
	'''
	p[0] = make_node(':')
	make_edge(p[0],p[1])
	make_edge(p[0],p[2])

def p_ExpressionStatement(p):
	'''ExpressionStatement : StatementExpression SEMICOLON
	'''
	p[0] = p[1]

def p_StatementExpression(p):
	'''StatementExpression : Assignment
						  | PreIncrementExpression
						  | PreDecrementExpression
						  | PostIncrementExpression
						  | PostDecrementExpression
						  | MethodInvocation
						  | ClassInstanceCreationExpression
	'''
	p[0] = p[1]

def p_IfThenStatement(p):
	'''IfThenStatement : if LBR Expression RBR Statement
	'''
	p[0] = make_node('IfThen')
	make_edge(p[0],p[3])
	make_edge(p[0],p[5])

def p_IfThenElseStatement(p):
	'''IfThenElseStatement : if LBR Expression RBR StatementNoShortIf else Statement
	'''
	p[0] = make_node('IfThenElse')
	make_edge(p[0],p[3])
	make_edge(p[0],p[5])
	make_edge(p[0],p[7])

def p_IfThenElseStatementNoShortIf(p):
	'''IfThenElseStatementNoShortIf : if LBR Expression RBR StatementNoShortIf else StatementNoShortIf
	'''
	p[0] = make_node('IfThenElse')
	make_edge(p[0],p[3])
	make_edge(p[0],p[5])
	make_edge(p[0],p[7])


def p_WhileStatement(p):
	'''WhileStatement : while LBR Expression RBR Statement
	'''
	p[0] = make_node('while')
	make_edge(p[0],p[3])
	make_edge(p[0],p[5])

def p_WhileStatementNoShortIf(p):
	'''WhileStatementNoShortIf : while LBR Expression RBR StatementNoShortIf
	'''
	p[0] = make_node('whileif')
	make_edge(p[0],p[3])
	make_edge(p[0],p[5])

def p_ForStatement(p):
	'''ForStatement : BasicForStatement
 				   | EnhancedForStatement
 	'''
	p[0] = p[1]

def p_ForStatementNoShortIf(p):
	'''ForStatementNoShortIf : BasicForStatementNoShortIf
							| EnhancedForStatementNoShortIf
	'''
	p[0] = p[1]

def p_BasicForStatement(p):
	'''BasicForStatement : for LBR SEMICOLON SEMICOLON RBR Statement
						| for LBR ForInit SEMICOLON SEMICOLON RBR Statement
						| for LBR SEMICOLON Expression SEMICOLON RBR Statement
						| for LBR SEMICOLON SEMICOLON ForUpdate RBR Statement
						| for LBR ForInit SEMICOLON Expression SEMICOLON RBR Statement
						| for LBR SEMICOLON Expression SEMICOLON ForUpdate RBR Statement
						| for LBR ForInit SEMICOLON SEMICOLON ForUpdate RBR Statement
						| for LBR ForInit SEMICOLON Expression SEMICOLON ForUpdate RBR Statement
	'''
	p[0] = make_node("for")
	if(len(p.slice)==7):
		temp1 = make_node(";")
		temp2 = make_node(";")
		make_edge(p[0],temp1)
		make_edge(p[0],temp2)
		make_edge(p[0],p[6])
	elif(len(p.slice)==8):
		if(str(p.slice[3])=="ForInit"):
			make_edge(p[0],p[3])
			temp1 = make_node(";")
			temp2 = make_node(";")
			make_edge(p[0],temp1)
			make_edge(p[0],temp2)
		elif(str(p.slice[4])=="Expression"):
			temp2 = make_node(";")
			make_edge(p[0],temp2)
			make_edge(p[0],p[3])
			temp1 = make_node(";")
			make_edge(p[0],temp1)
		else:
			temp1 = make_node(";")
			temp2 = make_node(";")
			make_edge(p[0],temp1)
			make_edge(p[0],temp2)
			make_edge(p[0],p[3])
		make_edge(p[0],p[7])

	elif(len(p.slice)==9):
		if(str(p.slice[5])=="Expression"):
			make_edge(p[0],p[3])
			temp2 = make_node(";")
			make_edge(p[0],temp2)
			make_edge(p[0],p[5])
			temp1 = make_node(";")
			make_edge(p[0],temp1)
		elif(str(p.slice[3])=="SEMICOLON"):
			temp2 = make_node(";")
			make_edge(p[0],temp2)
			make_edge(p[0],p[4])
			temp1 = make_node(";")
			make_edge(p[0],temp2)
			make_edge(p[0],p[6])
		else:
			make_edge(p[0],p[3])
			temp1 = make_node(";")
			temp2 = make_node(";")
			make_edge(p[0],temp1)
			make_edge(p[0],temp2)
			make_edge(p[0],p[6])
		make_edge(p[0],p[8])
	else:
		make_edge(p[0],p[3])
		temp2 = make_node(";")
		make_edge(p[0],temp2)
		make_edge(p[0],p[5])
		temp1 = make_node(";")
		make_edge(p[0],temp2)
		make_edge(p[0],p[7])
		make_edge(p[0],p[9])

	


		



def p_BasicForStatementNoShortIf(p):
	'''BasicForStatementNoShortIf : for LBR SEMICOLON SEMICOLON RBR StatementNoShortIf
								| for LBR ForInit SEMICOLON SEMICOLON RBR StatementNoShortIf
								| for LBR SEMICOLON Expression SEMICOLON RBR StatementNoShortIf
								| for LBR SEMICOLON SEMICOLON ForUpdate RBR StatementNoShortIf
								| for LBR ForInit SEMICOLON Expression SEMICOLON RBR StatementNoShortIf
								| for LBR SEMICOLON Expression SEMICOLON ForUpdate RBR StatementNoShortIf
								| for LBR ForInit SEMICOLON SEMICOLON ForUpdate RBR StatementNoShortIf
								| for LBR ForInit SEMICOLON Expression SEMICOLON ForUpdate RBR StatementNoShortIf
	'''
	p[0] = make_node("for")
	if(len(p.slice)==7):
		temp1 = make_node(";")
		temp2 = make_node(";")
		make_edge(p[0],temp1)
		make_edge(p[0],temp2)
		make_edge(p[0],p[6])
	elif(len(p.slice)==8):
		if(str(p.slice[3])=="ForInit"):
			make_edge(p[0],p[3])
			temp1 = make_node(";")
			temp2 = make_node(";")
			make_edge(p[0],temp1)
			make_edge(p[0],temp2)
		elif(str(p.slice[4])=="Expression"):
			temp2 = make_node(";")
			make_edge(p[0],temp2)
			make_edge(p[0],p[3])
			temp1 = make_node(";")
			make_edge(p[0],temp1)
		else:
			temp1 = make_node(";")
			temp2 = make_node(";")
			make_edge(p[0],temp1)
			make_edge(p[0],temp2)
			make_edge(p[0],p[3])
		make_edge(p[0],p[7])

	elif(len(p.slice)==9):
		if(str(p.slice[5])=="Expression"):
			make_edge(p[0],p[3])
			temp2 = make_node(";")
			make_edge(p[0],temp2)
			make_edge(p[0],p[5])
			temp1 = make_node(";")
			make_edge(p[0],temp1)
		elif(str(p.slice[3])=="SEMICOLON"):
			temp2 = make_node(";")
			make_edge(p[0],temp2)
			make_edge(p[0],p[4])
			temp1 = make_node(";")
			make_edge(p[0],temp2)
			make_edge(p[0],p[6])
		else:
			make_edge(p[0],p[3])
			temp1 = make_node(";")
			temp2 = make_node(";")
			make_edge(p[0],temp1)
			make_edge(p[0],temp2)
			make_edge(p[0],p[6])
		make_edge(p[0],p[8])
	else:
		make_edge(p[0],p[3])
		temp2 = make_node(";")
		make_edge(p[0],temp2)
		make_edge(p[0],p[5])
		temp1 = make_node(";")
		make_edge(p[0],temp2)
		make_edge(p[0],p[7])
		make_edge(p[0],p[9])

def p_ForInit(p):
	'''ForInit : StatementExpressionList
			  | LocalVariableDeclaration
	'''
	p[0] = p[1]

def p_ForUpdate(p):
	'''ForUpdate : StatementExpressionList
	'''
	p[0] = p[1]

def p_StatementExpressionList(p):
	'''StatementExpressionList : StatementExpression
							   | StatementExpression comStatementExpressionStar
 	'''
	if(len(p)==3):
		p[0] = p[1]
		p[0].append(p[2])
	else:
		p[0] = [p[1]]
def p_comStatementExpressionStar(p):
	'''comStatementExpressionStar : COMMA StatementExpression
								 | comStatementExpressionStar COMMA StatementExpression
	'''
	if(len(p)==4):
		p[0] = p[1]
		p[0].append(p[3])
	else:
		p[0] = [p[2]]

def p_EnhancedForStatement(p):
	'''EnhancedForStatement : for LBR Type VariableDeclaratorId COLON Expression RBR Statement
	'''
	p[0] = make_node("for")
	make_edge(p[0],p[3])
	make_edge(p[0],p[4])
	temp = make_node(":")
	make_edge(p[0],temp)
	make_edge(p[0],p[6])
	make_edge(p[0],p[8])


def p_EnhancedForStatementNoShortIf(p):
	'''EnhancedForStatementNoShortIf : for LBR Type VariableDeclaratorId COLON Expression RBR StatementNoShortIf
	'''
	p[0] = make_node("for")
	make_edge(p[0],p[3])
	make_edge(p[0],p[4])
	temp = make_node(":")
	make_edge(p[0],temp)
	make_edge(p[0],p[6])
	make_edge(p[0],p[8])


def p_BreakStatement(p):
	'''BreakStatement : break SEMICOLON
					 | break Identifier SEMICOLON
	'''
	p[0] = make_node('break')
	if(len(p.slice)==4):
		make_edge(p[0],p[2])

def p_ContinueStatement(p):
	'''ContinueStatement : continue SEMICOLON
						| continue Identifier SEMICOLON
	'''
	p[0] = make_node('continue')
	if(len(p.slice)==4):
		make_edge(p[0],p[2])

def p_ReturnStatement(p):
	'''ReturnStatement : return SEMICOLON
					  | return Expression SEMICOLON
	'''
	p[0] = make_node('return')
	if(len(p.slice)==4):
		make_edge(p[0],p[2])

def p_ThrowStatement(p):
	'''ThrowStatement : throw Expression SEMICOLON
	'''
	p[0] = make_node('throw')
	if(len(p.slice)==4):
		make_edge(p[0],p[2])


def p_TryStatement(p):
	'''TryStatement : try Block Catches
				   | try Block Finally
				   | try Block Catches Finally
	'''
	p[0] = make_node('try')
	make_edge(p[0],p[2])


def p_Catches(p):
	'''Catches : CatchClause
			   | Catches CatchClause
	'''
	if(len(p)==3):
		p[0] = p[1]
		p[0].append(p[2])
	else:
		p[0] = [p[1]]

def p_CatchClause(p):
	'''CatchClause : catch LBR FormalParameter RBR Block
	'''
	p[0] = make_node('catch')
	make_edge(p[0],p[3])
	make_edge(p[0],p[5])


def p_Finally(p):
	'''Finally : finally Block
	'''
	p[0] = make_node('finally')
	make_edge(p[0],p[2])

 # Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!",type(p))


 # Build the parser
parser = yacc.yacc()
args = sys.argv

outfile = "graph.dot"
input_file = open(args[1],'r')
input_str = input_file.read()

parser.parse(input_str)
dot.render(outfile)
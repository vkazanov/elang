import lib2to3

from lib2to3.pgen2 import pgen

if __name__ == "__main__":
	pg = pgen.ParserGenerator("grammar-cpython-1.6")
	first = pg.first
	for nonterm in first:
		print "%s ==>\t %s" % (nonterm, first[nonterm].keys())

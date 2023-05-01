# UEM-DIN-9797_COMPILADORES
Um repositório para os trabalhos e exercicios realizados para a conclusão da disciplina de compiladores da Universidade estadual de Maringá (UEM)

## Trabalho

O Trabalho foi realizado utilizando a linguagem de programação python 3.8
com o auxílio das bibliotecas ply e rply, a linguagem de programação alvo é ANSI C.
É válido ressaltar que que para a análise sintática foi utilizada uma gramática
disponível online para ANSI C o link para ela está disponível nas referências, assim
como todos os materiais que contribuíram para realização deste trabalho.

Para a implementação do analisador léxico foi realizada uma busca na
internet para as palavras reservadas da linguagem C, foi utilizado o padrão de
nomeação da gramática ANSI C para nomear cada token. Nesta parte a dificuldade
está relacionada com o aprendizado das bibliotecas ply e rply, a rply em especial é
muito mal documentada e ela é a que possui as funcionalidades mais importantes,
não adianta de nada ter a ferramenta se não souber usar.

Na análise sintática foi implementado cada regra da gramática ANSI C, a
maior dificuldade encontrada nesta parte foi a grande quantidade de regras a serem
implementadas, além disso formalismo foi utilizado, neste caso com os esquemas
de tradução dirigida pela Sintaxe (TDS), é possível fazer a implementação da
análise semântica diretamente nas regras da gramática na análise sintática, pois o
rply é baseado no ply que é baseado no lex e yacc. Não foi possível estabelecer
todas as regras semânticas e nem definir todos os tipos e operações possíveis na
AST que rply exige. 

Conclui-se que o erro está acontecendo pois não há tratamento sintático e
semântico o suficiente, o que foi realizado não está sendo implementado
corretamente pela falta de conhecimento da biblioteca rply. Fui forçado a usar
somente ply no final me fazendo começar do zero. Por fim, só foi possível realizar a
análise léxica e uma parte da semântica.

Funcionamento
Crie um arquivo com o nome “input.ansic” contendo o código desejado na mesma
pasta do código python e em seguida execute o comando:
python3 main_ra107115.py

# Referências

ANSI C Gramática:

● https://www.lysator.liu.se/c/ANSI-C-grammar-y.html

● https://www.quut.com/c/ANSI-C-grammar-y-2011.html

● https://gist.github.com/codebrainz/2933703

PLY:

● https://www.dabeaz.com/ply/

● https://github.com/dabeaz/ply

RPLY:
● https://llvmlite.readthedocs.io/en/latest/user-guide/ir/ir-builder.html?highlight=shift#llvmlite.ir.IRBuilder.shl

● https://llvm.org/docs/GettingStarted.html

● https://releases.llvm.org/download.html

Tutoriais e exemplos:

● https://www.dcce.ibilce.unesp.br/~aleardo/cursos/compila/lex.html

● https://medium.com/@marcelogdeandrade/writing-your-own-programming-language-and-compiler-with-python-a468970ae6df

● https://github.com/eng-assys/CompiladorPhyton

● https://www.dcce.ibilce.unesp.br/~aleardo/cursos/compila/lex.html

● https://www.youtube.com/watch?v=A35SigWZyqo&t=5s&ab_channel=Caf%C3%A9eComputa%C3%A7%C3%A3o

● https://www.youtube.com/watch?v=A35SigWZyqo&t=5s&ab_channel=Caf%C3%A9eComputa%C3%A7%C3%A3o

● https://github.com/Hemilibeatriz/Compilador

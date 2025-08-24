from app.models.schemas.course import CourseCreate
from app.models.schemas.exercise import ExerciseCreate
from app.models.schemas.publication import QuestionCreate, AnswerCreate
from app.models.schemas.quiz_pass import QuizPassCreate
from app.models.schemas.quiz import QuizCreate, QuizQuestionCreate, OptionCreate
from app.models.schemas.submission import SubmissionCreate
from app.models.schemas.subunit import SubunitCreate
from app.models.schemas.unit import UnitCreate
from app.models.schemas.user import UserCreate
from app.models.schemas.vote import VoteCreate

from app.db.crud import course as course_crud
from app.db.crud import exercise as exercise_crud
from app.db.crud import publication as publication_crud
from app.db.crud import quiz_pass as quiz_pass_crud 
from app.db.crud import quiz as quiz_crud
from app.db.crud import submission as submission_crud
from app.db.crud import subunit as subunit_crud
from app.db.crud import unit as unit_crud
from app.db.crud import user as user_crud
from app.db.crud import vote as vote_crud

from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.exc import IntegrityError

async def create_initial_data(db : AsyncSession) -> None:
  """Create initial data in the database."""
  user1 = UserCreate(
      username="user1",
      email="user1@example.com",
      password="password1"
  )
  user2 = UserCreate(
      username="user2",
      email="user2@example.com",
      password="password2"
  )
  user3 = UserCreate(
      username="user3",
      email="user3@example.com",
      password="password3"
  )
  try:
      await user_crud.create_user(db, user1)
      await user_crud.create_user(db, user2)
      await user_crud.create_user(db, user3)
  except IntegrityError:
      await db.rollback()
      pass

  course = CourseCreate(
      title="Tutorial de Erlang",
      description="Este es el tutorial de Erlang",
      )
  
  course_id = await course_crud.get_course(db, 1)
  if not course_id:
      course_db = await course_crud.create_course(db, course)
      course_id = course_db.id
  else:
      course_id = course_id.id

  unit1 = UnitCreate(
      title="Unidad 1: Fundamentos de Erlang y Pensamiento Funcional",
      description="This is the first unit of the course.",
      order=1,
      course_id=course_id
  )
  unit2 = UnitCreate(
      title="Unidad 2: Control de Flujo y Manejo de datos",
      description="This is the second unit of the course.",
      order=2,
      course_id=course_id
  )
  unit3 = UnitCreate(
      title="Unidad 3: Concurrencia y paso de mensajes",
      description="This is the third unit of the course.",
      order=3,
      course_id=course_id
  )

  unit1_db = await unit_crud.create_unit(db, unit1)
  unit2_db = await unit_crud.create_unit(db, unit2)
  unit3_db = await unit_crud.create_unit(db, unit3)
  unit1_id = unit1_db.id
  unit2_id = unit2_db.id
  unit3_id = unit3_db.id


  subunidad_1_parrafo = """<p>La primera duda que tendremos que responder es: <strong>¿Qué es Erlang?</strong><br>
Erlang es un lenguaje de programación de propósito general, concurrente y funcional. Fue desarrollado en 1986 en los Laboratorios de Ericsson con un objetivo muy concreto: crear software capaz de funcionar de forma ininterrumpida durante años, tolerar fallos sin colapsar y manejar una gran cantidad de usuarios simultáneos. Por eso, desde sus inicios se ha utilizado para sistemas de telecomunicaciones, donde la caída de un servicio puede tener un coste enorme.</p>

<p>Con el tiempo, Erlang se ha expandido más allá de las telecomunicaciones: hoy en día impulsa sistemas distribuidos, bases de datos como CouchDB, plataformas de mensajería y aplicaciones web en las que la fiabilidad y la concurrencia son esenciales.</p>

<p>La idea de este tutorial es proporcionar una introducción práctica a Erlang: entenderás sus conceptos fundamentales, su sintaxis y sus características clave, para que puedas escribir tus primeros programas y comprender por qué sigue siendo relevante casi 40 años después de su creación.</p>

<h3>¿Por qué estudiar Erlang?</h3>

<p>Erlang destaca por su capacidad para construir sistemas concurrentes, distribuidos y tolerantes a fallos. Su modelo de actores, su máquina virtual BEAM y su filosofía de “Let it crash!” facilitan la creación de aplicaciones que requieren alta disponibilidad y escalabilidad. Además, su enfoque funcional fomenta un código más claro y predecible.</p>
  
<p>Aunque no es un lenguaje masivo en popularidad, ha demostrado una eficacia sobresaliente. Un caso emblemático es WhatsApp: su backend, desarrollado originalmente en Erlang, fue capaz de manejar millones de conexiones concurrentes con un equipo de ingenieros sorprendentemente pequeño, gracias a la eficiencia del lenguaje y su robusto modelo de concurrencia.</p>

<p>En este tutorial no solo veremos cómo escribir código en Erlang, sino también cómo pensar en términos de procesos, mensajes y resiliencia, tal como lo hacen los sistemas que no pueden darse el lujo de fallar.</p>

<h3>Comparación de la sintaxis de Erlang con otros lenguajes</h3>

<p>
Erlang fue diseñado para construir sistemas altamente concurrentes, distribuidos y tolerantes a fallos. Su sintaxis difiere significativamente de lenguajes como Python, JavaScript o Java. Las sentencias en Erlang terminan con un punto (<text-code>.</text-code>) en lugar de punto y coma o saltos de línea. Las variables son inmutables una vez asignadas, a diferencia de la mayoría de los otros lenguajes.
</p>

<p>
Erlang se basa en el <em>pattern matching</em> y en construcciones funcionales en lugar de bucles y estado mutable. Los bloques de código se definen usando palabras clave (<text-code>if</text-code>, <text-code>case</text-code>, <text-code>fun</text-code>) en lugar de indentación o llaves. Este enfoque funcional fomenta un código declarativo y predecible, haciendo énfasis en el paso de mensajes y la concurrencia de procesos.
</p>

<h3>Un vistazo rápido a la sintaxis</h3>
"""
  subunidad_1_codigo = """% Comentario de una línea
-module(saludos).       % Nombre del módulo
-export([start/0,hola/1]).      % Funciones públicas

hola(Nombre) ->
  io:format("Hola, ~s!~n", [Nombre]).

start() ->
  hola("Estudiante").
"""

  subunidad_1_2_parrafo="""<ul>
  <li>Claves rápidas:
      <ul>
      <li>Las variables empiezan con mayúscula (<text-code>Nombre</text-code>).</li>
      <li>Los átomos (un tipo de dato que se explicará más tarde) van en minúscula (<text-code>ok</text-code>, <text-code>error</text-code>).</li>
      <li>Cada sentencia termina con <text-code>.</text-code></li>
      <li>Las variables son inmutables.</li>
      </ul>
  </li>
  </ul>
  """

  subunit1_1 = SubunitCreate(
      title="Introducción",
      description="This is the first subunit of unit 1.",
      order=1,
      blocks=[
        { "type": "text", "value": subunidad_1_parrafo },
        { "type": "code", "value": subunidad_1_codigo},
        { "type": "text", "value": subunidad_1_2_parrafo},
      ],
      unit_id=unit1_id
  )

  instalacion_erlang = """
<h3>1. En Linux</h3>
<h4>Distribuciones basadas en Debian/Ubuntu</h4>
<p><strong>1. Actualizar repositorios:</strong></p>
<pre><code class="language-bash">sudo apt update</code></pre>

<p><strong>2. Instalar Erlang:</strong></p>
<pre><code class="language-bash">sudo apt install erlang</code></pre>

<p><strong>3. Verificar instalación:</strong></p>
<pre><code class="language-bash">erl</code></pre>
<p>Debería abrir el shell interactivo de Erlang.</p>

<hr>

<h4>Distribuciones basadas en Fedora/CentOS</h4>
<p><strong>1. Actualizar repositorios:</strong></p>
<pre><code class="language-bash">sudo dnf update</code></pre>

<p><strong>2. Instalar Erlang:</strong></p>
<pre><code class="language-bash">sudo dnf install erlang</code></pre>

<p><strong>3. Verificar:</strong></p>
<pre><code class="language-bash">erl</code></pre>

<h3>2. En macOS</h3>
<p>La forma más sencilla es usando Homebrew.</p>

<p><strong>1. Instalar Homebrew (si no lo tienes):</strong></p>
<pre><code class="language-bash">/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"</code></pre>

<p><strong>2. Actualizar Homebrew:</strong></p>
<pre><code class="language-bash">brew update</code></pre>

<p><strong>3. Instalar Erlang:</strong></p>
<pre><code class="language-bash">brew install erlang</code></pre>

<p><strong>4. Verificar:</strong></p>
<pre><code class="language-bash">erl</code></pre>

<h3>3. En Windows</h3>
<h4>Opción 1: Instalar usando el instalador oficial</h4>
<ol>
<li>Ve a la página oficial: <a href="https://www.erlang.org/downloads">https://www.erlang.org/downloads</a></li>
<li>Descarga el instalador para Windows (archivo .exe).</li>
<li>Ejecuta el instalador y sigue los pasos del asistente.</li>
<li>Una vez instalado, abre PowerShell o CMD y ejecuta:</li>
</ol>
<pre><code class="language-c">erl</code></pre>
<p>para abrir el shell de Erlang.</p>

<h4>Opción 2: Usar Chocolatey (administrador de paquetes para Windows)</h4>
<ol>
<li>Abre PowerShell como administrador.</li>
<li>Instala Chocolatey (si no lo tienes):</li>
</ol>
<pre><code class="language-powershell">Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))</code></pre>

<ol start="3">
<li>Instala Erlang:</li>
</ol>
<pre><code class="language-powershell">choco install erlang</code></pre>

<ol start="4">
<li>Verifica la instalación:</li>
</ol>
<pre><code class="language-powershell">erl</code></pre>

<h3>Notas adicionales</h3>
<p>Para proyectos en Elixir o con herramientas como RabbitMQ, a veces se recomienda instalar versiones específicas de Erlang desde repositorios oficiales o usando asdf (un gestor de versiones).</p>

<p>Si quieres usar asdf para gestionar versiones de Erlang en Linux/macOS, puedo hacerte un tutorial aparte.</p>
"""
  subunit1_2 = SubunitCreate(
      title="Instalación de Erlang",
      description="This is the second subunit of unit 1.",
      order=2,
      blocks=[
          {"type": "text", "value": instalacion_erlang},
      ],
      unit_id=unit1_id
  )


  shell_comandos = """<h3>1. Qué es la Shell de Erlang</h3>
<p>
La Shell de Erlang es normalmente usada para probar expresiones. Por eso, las pruebas pueden realizarse fácilmente en la shell antes de ser probada en la propia aplicación.
</p>
<p>
Después de instalar Erlang en nuestra máquina, podemos abrir la shell desde la terminal escribiendo:
</p>
<pre><code class="language-bash">erl</code></pre>
<p>
En el caso de Windows, puedes utilizar <text-code>erl.exe</text-code> pero es mejor utilizar <text-code>werl.exe</text-code> que puede encontrarse en el menú de inicio (<text-code>C:\\Program Files\\erl&lt;version&gt;\\bin\\werl.exe</text-code>). Werl es una implementación individual de la shell de Erlang para Windows. Tiene su propia ventana con barras de desplazamiento y admite la edición desde la línea de comandos (como copiar y pegar, lo cual se volvió un problema con el shell <text-code>cmd.exe</text-code> estándar de Windows). El shell ERL sigue siendo necesario si desea redirigir la entrada o salida estándar, o usar pipelines.
</p>
<p>y se verá algo así:</p>
<pre><code class="language-bash">Erlang/OTP 28 [erts-16.0.2]
Eshell V16.0.2 (press Ctrl+G to abort, type help(). for help)
1&gt;</code></pre>
<p>El prompt <text-code>1&gt;</text-code> indica que la shell está lista para recibir comandos.</p>

<hr>

<h3>2. Comandos básicos de la shell</h3>

<h4>2.1 Funcionalidad</h4>
<p>
El shell de Erlang tiene un editor de líneas integrado basado en un subconjunto de Emacs, un editor de texto popular que se usa desde los años 70. Si conoces Emacs, no tendrás ningún problema. Para los demás, te servirá de todas formas.
</p>
<p>
Primeramente, al escribir un texto, si quieres ir al inicio de la línea puedes utilizar <text-code>^A</text-code> (Ctrl+A), en caso contrario si quieres ir al final de la línea utiliza <text-code>^E</text-code>. También para moverte alrededor del texto utiliza las teclas de flecha horizontales, y en caso de querer utilizar comandos anteriores o mostrar siguientes líneas para repetir código utiliza las teclas de flecha verticales.
</p>
<p>
Si escribes algo como <text-code>li</text-code> y pulsas <kbd>Tab</kbd>, el shell mostrará las funciones y módulos disponibles que se pueden utilizar que empiezan por <text-code>li</text-code>. Si completas con el módulo <text-code>lists:</text-code> y pulsas <kbd>Tab</kbd> otra vez se mostrarán todos los métodos que ofrece este módulo.
</p>

<h4>2.2 Comandos de control</h4>
<table>
<thead>
<tr><th>Comando</th><th>Función</th></tr>
</thead>
<tbody>
<tr><td><text-code>^A</text-code> (Ctrl+A)</td><td>Ver el cursor en el principio de la línea</td></tr>
<tr><td><text-code>^E</text-code></td><td>Ver el cursor al final de la línea</td></tr>
<tr><td><text-code>b().</text-code></td><td>Imprime todos los enlaces de variables actuales. <strong>Nota: el punto es obligatorio</strong>.</td></tr>
<tr><td><text-code>q().</text-code></td><td>Salir de la shell.</td></tr>
<tr><td><text-code>f().</text-code></td><td>Elimina todos los enlaces de variables actuales.</td></tr>
<tr><td><text-code>c(Modulo).</text-code></td><td>Compila un módulo Erlang. Ejemplo: <text-code>c(mi_modulo).</text-code></td></tr>
<tr><td><text-code>l(Modulo).</text-code></td><td>Recarga un módulo ya compilado.</td></tr>
<tr><td><text-code>exit()</text-code></td><td>Finaliza la sesión</td></tr>
<tr><td><text-code>process_info(Pid).</text-code></td><td>Información sobre un proceso específico.</td></tr>
</tbody>
</table>
<p>
En Erlang, <strong>todos los comandos deben terminar en <text-code>.</text-code></strong> y presionar Enter para ejecutarlos. En caso de querer buscar más comandos utiliza <text-code>help().</text-code>.
</p>
<p>
Si prestaste atención, al iniciar el shell, había un comentario sobre <text-code>press Ctrl+G to abort</text-code>. ¡Hagámoslo y luego presionemos <text-code>h</text-code> para obtener ayuda!
</p>
<pre><code class="language-erlang">User switch command (enter 'h' for help)
--> h

c [nn]            - connect to job
i [nn]            - interrupt job
k [nn]            - kill job
j                 - list all jobs
s [shell]         - start local shell
r [node [shell]]  - start remote shell
q                 - quit erlang
? | h             - this message
</code></pre>
<p>
Esto cambiará la shell al modo de control de trabajos, que permite crear sesiones multishell, cerrar otras sesiones o incluso iniciar un shell remoto.
</p>

<hr>

<h3>3. Operaciones básicas en la shell</h3>

<h4>3.1 Operaciones aritméticas</h4>
<p>Erlang soporta operaciones básicas con sintaxis similar a otros lenguajes:</p>
<table>
<thead><tr><th>Operación</th><th>Ejemplo</th><th>Resultado</th></tr></thead>
<tbody>
<tr><td>Suma</td><td><text-code>5 + 3.</text-code></td><td><text-code>8</text-code></td></tr>
<tr><td>Resta</td><td><text-code>10 - 4.</text-code></td><td><text-code>6</text-code></td></tr>
<tr><td>Multiplicación</td><td><text-code>7 * 6.</text-code></td><td><text-code>42</text-code></td></tr>
<tr><td>División (float)</td><td><text-code>10 / 4.</text-code></td><td><text-code>2.5</text-code></td></tr>
<tr><td>División entera</td><td><text-code>10 div 4.</text-code></td><td><text-code>2</text-code></td></tr>
<tr><td>Resto</td><td><text-code>10 rem 4.</text-code></td><td><text-code>2</text-code></td></tr>
</tbody>
</table>
<pre><code class="language-erlang">1> 10 + 5.
15
2> 10 / 4.
2.5
3> 10 div 4.
2
</code></pre>

<h4>3.2 Operaciones de comparación</h4>
<table>
<thead><tr><th>Operación</th><th>Ejemplo</th><th>Resultado</th></tr></thead>
<tbody>
<tr><td>Igual</td><td><text-code>5 == 5.</text-code></td><td><text-code>true</text-code></td></tr>
<tr><td>Distinto</td><td><text-code>5 /= 3.</text-code></td><td><text-code>true</text-code></td></tr>
<tr><td>Mayor que</td><td><text-code>7 > 3.</text-code></td><td><text-code>true</text-code></td></tr>
<tr><td>Menor que</td><td><text-code>2 < 5.</text-code></td><td><text-code>true</text-code></td></tr>
<tr><td>Mayor o igual</td><td><text-code>7 >= 7.</text-code></td><td><text-code>true</text-code></td></tr>
<tr><td>Menor o igual</td><td><text-code>3 =< 3.</text-code></td><td><text-code>true</text-code></td></tr>
</tbody>
</table>
<pre><code class="language-erlang">1> 5 == 10.
false
2> 10 /= 4.
true
3> 10 > 4.
true
</code></pre>

<h4>3.3 Operaciones lógicas</h4>
<table>
<thead><tr><th>Operador</th><th>Ejemplo</th><th>Resultado</th></tr></thead>
<tbody>
<tr><td>AND lógico</td><td><text-code>true and false.</text-code></td><td><text-code>false</text-code></td></tr>
<tr><td>OR lógico</td><td><text-code>true or false.</text-code></td><td><text-code>true</text-code></td></tr>
<tr><td>NOT lógico</td><td><text-code>not true.</text-code></td><td><text-code>false</text-code></td></tr>
<tr><td>AND cortocircuito</td><td><text-code>true andalso (2 > 1).</text-code></td><td><text-code>true</text-code></td></tr>
<tr><td>OR cortocircuito</td><td><text-code>false orelse true.</text-code></td><td><text-code>true</text-code></td></tr>
</tbody>
</table>
<pre><code class="language-erlang">1> true and false.
false
2> true or false.
true
3> not true.
false
</code></pre>

<h4>3.4 Definición de variables</h4>
<pre><code class="language-erlang">X = 10.       % OK
X = 10.       % OK
X = 20.       % Error (ya asignada)
</code></pre>

<h4>3.5 Listas y tuplas</h4>
<pre><code class="language-erlang">[1,2,3] ++ [4,5].   % -> [1,2,3,4,5]
[1,2,3] -- [2].     % -> [1,3]
hd([1,2,3]).        % -> 1
tl([1,2,3]).        % -> [2,3]

{ok, 200}.          % Tupla
element(1, {ok,200}). % -> ok
</code></pre>

<h4>3.6 Funciones rápidas en la shell</h4>
<pre><code class="language-erlang">Square = fun(X) -> X * X end.
Square(5).   % -> 25

lists:reverse([1,2,3]). % -> [3,2,1]
</code></pre>"""
  
  subunit1_3 = SubunitCreate(
      title="Shell, comandos y operaciones",
      description="This is the third subunit of unit 1.",
      order=3,
      blocks=[
          {"type": "text", "value": shell_comandos},
      ],
      unit_id=unit1_id
  )

  tipos_datos = """  <p>En Erlang, todo gira en torno a unos pocos tipos de datos fundamentales. Conocerlos bien es esencial para entender cómo funciona el lenguaje y cómo estructurar programas robustos y expresivos. A continuación se describen los más importantes.</p>

<h3>Enteros</h3>
<p>Los enteros en Erlang son números sin parte decimal. Pueden ser positivos o negativos, y no tienen un límite fijo más allá de la memoria disponible.</p>
<pre><code class="erlang">1       % entero positivo
-42     % entero negativo
1234567890</code></pre>

<p>Puedes usarlos en operaciones aritméticas comunes:</p>
<pre><code class="erlang">2 + 3    % 5
10 * -2  % -20</code></pre>

<p>También podemos utilizar varios operadores en una sola expresión, y las operaciones matemáticas obedecen las reglas de precedencia normales.</p>
<pre><code class="erlang">1> (50 * 100) - 4999.
5001
2> -(50 * 100 - 4999).
-5001
3> -50 * (100 - 4999).
244950</code></pre>

<p>Si deseas expresar números enteros en bases distintas a la base 10, simplemente ingresa el número como <text-code>Base#Valor</text-code> (siempre que la Base esté en el rango 2..36):</p>
<pre><code class="erlang">4> 2#101010.
42
5> 8#0677.
447
6> 16#AE.
174</code></pre>

<h3>Variables</h3>
<p>Una de las características destacables de Erlang es el manejo de variables. Primero que todo, cualquier variable que creemos se mantendrá invariable, o mejor dicho, inmutable; no se podrá cambiar su valor. El comportamiento básico de las variables se puede demostrar con estas 7 expresiones (ten en cuenta que las variables comienzan con una letra mayúscula):</p>
<pre><code class="erlang">1> One.
* 1: variable 'One' is unbound
2> One = 1.
1
3> Un = Uno = One = 1.
1
4> Two = One + One.
2
5> Two = 2.
2
6> Two = Two + 1.
** exception error: no match of right hand side value 3
7> two = 2.
** exception error: no match of right hand side value 2</code></pre>

<p>Lo primero que nos indican estos comandos es que se puede asignar un valor a una variable solo una vez; luego, se puede simular que se le asigna un valor si es el mismo que ya tiene. Si es diferente, Erlang generará una excepción. Esta observación depende del operador <text-code>=</text-code>. El operador <text-code>=</text-code> compara valores y genera una excepción si son diferentes. Si son iguales, devuelve el valor:</p>
<pre><code class="erlang">8> 47 = 45 + 2.
47
9> 47 = 45 + 3.
** exception error: no match of right hand side value 48</code></pre>

<p>Este comportamiento del operador <text-code>=</text-code> es la base del <strong>pattern matching</strong>, característica de muchos lenguajes de programación funcional. Veremos el pattern matching con más detalle al analizar tuplas, listas y funciones.</p>
<pre><code class="erlang">10> _ = 14+3.
17
11> _.
* 1: variable '_' is unbound</code></pre>

<h3>Átomos</h3>
<p>Los nombres de las variables no pueden empezar con minúscula: los átomos. Los átomos son literales, constantes con su propio nombre como valor. Lo que ves es lo que hay, no esperes más. Por ejemplo, el átomo <text-code>gato</text-code> significa "gato" y nada más.</p>
<pre><code class="erlang">1> atom.
atom
2> atoms_rule.
atoms_rule
3> atoms_rule@erlang.
atoms_rule@erlang
4> 'Atoms can be cheated!'.
'Atoms can be cheated!'
5> atom = 'atom'.
atom</code></pre>

<p>Una forma de entender los átomos es tomarlos como constantes que utilizan su nombre como valor. Por ejemplo, para representar colores se podrían usar <text-code>azul</text-code>, <text-code>marron</text-code>, <text-code>verde</text-code>, <text-code>otro</text-code>, y se pueden utilizar en cualquier parte del código.</p>

<p>Nota: algunos átomos son palabras reservadas y no se pueden usar excepto para lo que los diseñadores del lenguaje definieron: <text-code>after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let not of or orelse query receive rem try when xor</text-code>.</p>

<h3>Booleanos</h3>
<p>No existe un tipo booleano independiente: se usan los átomos <text-code>true</text-code> y <text-code>false</text-code> para representar valores lógicos.</p>
<pre><code class="erlang">1> true and false.
false
2> false or true.
true
3> true xor false.
true
4> not false.
true
5> not (true and true).
false</code></pre>

<p>Probar la igualdad o desigualdad también es sencillo:</p>
<pre><code class="erlang">6> 5 =:= 5.
true
7> 1 =:= 0.
false
8> 1 =/= 0.
true
9> 5 =:= 5.0.
false
10> 5 == 5.0.
true
11> 5 /= 5.0.
false</code></pre>

<p>Otros operadores de comparación:</p>
<pre><code class="erlang">12> 1 < 2.
true
13> 1 < 1.
false
14> 1 >= 1.
true
15> 1 =< 1.
true</code></pre>

<h2>Tuplas</h2>
<p>Una <strong>tupla</strong> es una forma de organizar datos agrupando varios elementos cuando se conoce su número.  
En Erlang, una tupla se escribe:</p>
<pre><code class="erlang">{Elemento1, Elemento2, ..., ElementoN}</code></pre>

<p>Ejemplo:</p>
<pre><code class="erlang">1> X = 10, Y = 4.
10
2> Point = {X, Y}.
{10,4}</code></pre>

<h3>Extrayendo valores de una tupla</h3>
<pre><code class="erlang">3> f().
ok
4> Point = {4,5}.
{4,5}
5> {X, Y} = Point.
{4,5}
6> X.
4</code></pre>

<h3>Ignorando valores con _</h3>
<pre><code class="erlang">7> {X, _} = Point.
{4,5}
8> X.
4</code></pre>

<h3>Tuplas etiquetadas</h3>
<pre><code class="erlang">11> Temperature = 23.213.
23.213
12> PreciseTemperature = {celsius, 23.213}.
{celsius,23.213}
13> {kelvin, T} = PreciseTemperature.
** exception error: no match of right hand side value {celsius,23.213}</code></pre>

<h3>Tuplas anidadas</h3>
<pre><code class="erlang">14> {point, {X, Y}}.
{point,{4,5}}</code></pre>

<h2>Listas</h2>
<p>Las <strong>listas</strong> pueden contener cualquier tipo de dato:</p>
<pre><code class="erlang">[Elemento1, Elemento2, ..., ElementoN]</code></pre>

<p>Ejemplo:</p>
<pre><code class="erlang">1> [1, 2, 3, {numbers,[4,5,6]}, 5.34, atom].
[1,2,3,{numbers,[4,5,6]},5.34,atom]</code></pre>

<h3>Listas y cadenas</h3>
<pre><code class="erlang">2> [97, 98, 99].
"abc"
3> [97,98,99,4,5,6].
[97,98,99,4,5,6]
4> [233].
"é"</code></pre>

<h3>Concatenación y eliminación de elementos</h3>
<pre><code class="erlang">5> [1,2,3] ++ [4,5].
[1,2,3,4,5]
6> [1,2,3,4,5] -- [1,2,3].
[4,5]
7> [2,4,2] -- [2,4].
[2]
8> [2,4,2] -- [2,4,2].
[]</code></pre>

<h3>Cabeza y cola de una lista</h3>
<pre><code class="erlang">11> hd([1,2,3,4]).
1
12> tl([1,2,3,4]).
[2,3,4]</code></pre>

<h3>Pattern matching con listas</h3>
<pre><code class="erlang">13> List = [2,3,4].
[2,3,4]
14> NewList = [1 | List].
[1,2,3,4]
15> [Head|Tail] = NewList.
[1,2,3,4]
16> Head.
1
17> Tail.
[2,3,4]</code></pre>

<h3>Formas equivalentes de declarar listas</h3>
<pre><code class="erlang">[a, b, c, d]
[a, b, c, d | []]
[a, b | [c, d]]
[a, b | [c | [d]]]
[a | [b | [c | [d]]]]
[a | [b | [c | [d | []]]]]</code></pre>

<h3>Listas impropias</h3>
<p>Una lista como <text-code>[1 | 2]</text-code> se llama <strong>lista impropia</strong> y no es compatible con funciones estándar como <text-code>length/1</text-code>. Erlang espera listas propias, que terminan siempre con <text-code>[]</text-code>.</p>

<h2>Comprensiones de Listas</h2>
<h3>Comprensiones básicas</h3>
<pre><code class="erlang">1> [2*N || N <- [1,2,3,4,5]].
[2,4,6,8,10]</code></pre>

<h3>Con guardas</h3>
<pre><code class="erlang">2> [X*X || X <- [1,2,3,4,5], X rem 2 == 0].
[4,16]</code></pre>

<h3>Varias listas</h3>
<pre><code class="erlang">3> [X+Y || X <- [1,2,3], Y <- [10,20]].
[11,21,12,22,13,23]</code></pre>
"""

  subunit1_4 = SubunitCreate(
      title="Tipos de datos en Erlang",
      description="This is the fourth subunit of unit 1.",
      order=4,
      blocks=[
          {"type": "text", "value": tipos_datos}
      ],
      unit_id=unit1_id
  )

  modulos = """<h2>Qué Son los Módulos</h2>
  <p>Un <strong>módulo</strong> es un archivo que contiene un grupo de funciones bajo un solo nombre.<br>
  Trabajar con el shell interactivo es útil para probar código, pero el código necesita ser guardado para poder reutilizarlo; aquí es donde entran los módulos.</p>

  <ul>
    <li>Todas las funciones en Erlang deben definirse dentro de módulos.</li>
    <li>Las funciones integradas (BIFs) como <text-code>hd/1</text-code> o <text-code>tl/1</text-code> pertenecen al módulo <text-code>erlang</text-code> y se importan automáticamente.</li>
    <li>Otras funciones requieren la forma <text-code>Modulo:Funcion(Argumentos)</text-code> para ser llamadas.</li>
  </ul>

  <p><strong>Ejemplo:</strong></p>
  <pre><code>1&gt; erlang:element(2, {a,b,c}).
b
2&gt; element(2, {a,b,c}).
b
3&gt; lists:seq(1,4).
[1,2,3,4]
4&gt; seq(1,4).
** exception error: undefined shell command seq/2
</code></pre>

  <ul>
    <li>Usa una separación lógica: las funciones sobre listas van en el módulo <text-code>lists</text-code>, funciones de entrada/salida en el módulo <text-code>io</text-code>.</li>
    <li>Evita crear módulos como <text-code>erlang</text-code> con funciones no relacionadas.</li>
  </ul>

  <h2>Declaración de Módulo</h2>
  <p>Los módulos contienen <strong>funciones</strong> y <strong>atributos</strong> (metadatos).</p>

  <ul>
    <li>El único atributo obligatorio para compilar es el nombre del módulo:
      <pre><code>-module(Nombre).</code></pre>
    </li>
    <li>Las funciones exportadas definen la interfaz del módulo:
      <pre><code>-export([Funcion1/Aridad, Funcion2/Aridad, ..., FuncionN/Aridad]).</code></pre>
    </li>
  </ul>

  <h3>Ejemplo: Módulo <text-code>useless.erl</text-code></h3>"""

  uselesserl = """-module(useless).
-export([add/2, hello/0, greet_and_add_two/1,start/0]).

add(A,B) ->
  A + B.

%% Muestra un saludo.
hello() ->
  io:format("¡Hola, mundo!~n").

greet_and_add_two(X) ->
  hello(),
  add(X,2).
  
start() ->
  hello(),
  io:format("Sum 5 + 2: ~p~n",[add(5,2)]).
"""

  siguiente = """
  <ul>
    <li>Los comentarios usan <text-code>%</text-code> para líneas individuales.</li>
    <li>Las funciones dentro de un módulo pueden llamarse entre sí sin anteponer el nombre del módulo.</li>
    <li>Opcionalmente, se pueden importar funciones de otros módulos:
      <pre><code>-import(Modulo, [Funcion1/Aridad, ..., FuncionN/Aridad]).</code></pre>
    </li>
  </ul>

  <h2>Compilación de Código</h2>
  <p>El código de Erlang se compila a bytecode (<text-code>.beam</text-code>) para la máquina virtual.</p>

  <p><strong>Pasos en el Shell:</strong></p>
  <pre><code>1&gt; cd("/ruta/del/modulo/").
"Nombre de la ruta al directorio"
ok
2&gt; c(useless).
{ok,useless}
3&gt; useless:add(7,2).
9
4&gt; useless:hello().
¡Hola, mundo!
ok
5&gt; useless:greet_and_add_two(-3).
¡Hola, mundo!
-1
6&gt; useless:start().
¡Hola, mundo!
7
7&gt; useless:not_a_real_function().
** exception error: undefined function useless:not_a_real_function/0
</code></pre>

  <ul>
    <li>Los archivos <text-code>.beam</text-code> contienen bytecode compilado.</li>
    <li>Ejemplos de flags de compilación:
      <pre><code>7&gt; compile:file(useless, [debug_info, export_all]).
{ok,useless}
8&gt; c(useless, [debug_info, export_all]).
{ok,useless}</code></pre>
    </li>
    <li>También se pueden incluir flags de compilación dentro del módulo:
      <pre><code>-compile([debug_info, export_all]).</code></pre>
    </li>
    <li>La compilación nativa está disponible en algunas plataformas usando <text-code>hipe:c(Modulo,ListaDeOpciones)</text-code> o <text-code>c(Modulo,[native])</text-code>.</li>
  </ul>"""

  subunit1_5 = SubunitCreate(
      title="Módulos",
      description="This is the fifth subunit of unit 1.",
      order=5,
      blocks=[
          {"type": "text", "value": modulos},
          {"type": "code", "value": uselesserl},
          {"type" : "text", "value": siguiente}
      ],
      unit_id=unit1_id
  )

  info1 = """ <h2>Patrones</h2>

  <h3>Pattern Matching en Funciones</h3>
  <p>Para evitar escribir este tipo de funciones en erlang:</p>
      <pre><code>function greet(Gender,Name)
  if Gender == male then
      print("Hello, Mr. %s!", Name)
  else if Gender == female then
      print("Hello, Mrs. %s!", Name)
  else
      print("Hello, %s!", Name)
end</code></pre>
  <p>Usamos el Pattern Matching para simplificar la función, lo que nos ahorra escribir un código repetitivo:</p>"""

  code1 = """-module(greet).
-compile(export_all).

greet(male, Name) ->
  io:format("Hello, Mr. ~s!~n", [Name]);
greet(female, Name) ->
  io:format("Hello, Mrs. ~s!~n", [Name]);
greet(_, Name) ->
  io:format("Hello, ~s!~n", [Name]).
start() ->
  greet(male, "John"),
  greet(female, "Jane"),
  greet(other, "Alex").
"""

  info2 = """"<p><strong>Notas:</strong></p>
  <ul>
    <li>Cada cláusula de función es llamada <em>function clause</em>.</li>
    <li>La cláusula final termina con un punto.</li>
    <li><text-code>io:format</text-code> usa tokens como <text-code>~s</text-code>, <text-code>~p</text-code>, <text-code>~n</text-code>.</li>
  </ul>

  <h3>Pattern Matching en Listas</h3>
  <p>El Pattern Matching en funciones puede ser más compleja y potente. Como quizás recuerdes de capítulos anteriores, podemos hacer Pattern Matching en listas para obtener caras y cruces. ¡Hagámoslo!</p>
  <p>Ejemplos de funciones para obtener elementos de una lista:</p>"""

  code2 = """-module(functions).
-compile(export_all). % Sustituyendo export() para no colocar uno por uno

head([H|_]) -> H.
second([_,X|_]) -> X.
same(X,X) -> true;
same(_,_) -> false.
start() -> 
  io:format("Head: ~p~n", [head([1,2,3,4])]),
  io:format("Second: ~p~n", [second([1,2,3,4])]),
  io:format("Same (a,a): ~p~n", [same(a,a)]),
  io:format("Same (a,b): ~p~n", [same(a,b)]).
"""

  info3 = """    <h3>Pattern Matching con Tuplas</h3>
  <p>Funciona con cualquier tipo de datos, no solo listas o variables individuales. Como otro ejemplo, la siguiente función imprime una fecha, pero solo si tiene el formato correcto.</p>"""

  code3 = """-module(date).
-compile(export_all).

valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
  io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
  io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
  io:format("Stop feeding me wrong data!~n").
start() ->
  valid_time({{2011,09,06},{09,04,43}}),
  valid_time({{2011,09,06},{09,04}}).
  """

  info4 = """<p>Tenga en cuenta que es posible usar el operador <text-code>=</text-code> en la cabecera de la función, lo que nos permite hacer coincidir tanto el contenido dentro de una tupla <text-code>({A,M,D})</text-code> como la tupla completa <text-code>(Fecha)</text-code>.</p>

  <hr>

  <h2>Guardas</h2>

  <h3>Funciones con Guardas</h3>
  <p>Evitar escribir muchas cláusulas:</p>
  """

  code4 = """-module(guards).
-compile(export_all).

old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.
right_age(X) when X >= 16, X =< 104 -> true; % Rango de edad válido
right_age(_) -> false.
wrong_age(X) when X < 16; X > 104 -> true; % Función inversa
wrong_age(_) -> false.

start() ->
  io:format("Is 17 old enough? ~p~n", [old_enough(17)]),
  io:format("Is 15 old enough? ~p~n", [old_enough(15)]),
  io:format("Is 30 the right age? ~p~n", [right_age(30)]),
  io:format("Is 15 the right age? ~p~n", [right_age(15)]),
  io:format("Is 105 the wrong age? ~p~n", [wrong_age(105)]),
  io:format("Is 50 the wrong age? ~p~n", [wrong_age(50)]).
"""

  info5 = """   <p><strong>Notas:</strong></p>
  <ul>
    <li><text-code>,</text-code> equivale a <text-code>andalso</text-code> y <text-code>;</text-code> equivale a <text-code>orelse</text-code>.</li>
    <li>Solo funciones internas y operaciones permitidas pueden usarse en guardas.</li>
  </ul>

  <hr>

  <h2>Sentencia if</h2>
  <p>Erlang <text-code>if</text-code> es similar a guardas, pero fuera del encabezado de la función.</p>

  <h3>Ejemplo básico</h3>
  """

  code5 = """-module(what_the_if).
-compile(export_all).

oh_god(N) ->
  if N =:= 2 -> might_succeed;
      true -> always_does
  end.

start() ->
  io:format("Result for 2: ~p~n", [oh_god(2)]),
  io:format("Result for 3: ~p~n", [oh_god(3)]).
"""

  info6 = """<h3>Múltiples condiciones</h3>"""

  code6 = """-module(animals).
-compile(export_all).
  
help_me(Animal) ->
  Talk = if Animal == cat  -> "meow";
            Animal == beef -> "mooo";
            Animal == dog  -> "bark";
            Animal == tree -> "bark";
            true -> "fgdadfgna"
          end,
  {Animal, "says " ++ Talk ++ "!"}.

start() ->
  io:format("~p~n", [help_me(dog)]),
  io:format("~p~n", [help_me("it hurts!")]).
"""

  info7 = """<p><strong>Nota:</strong> <text-code>true</text-code> actúa como <em>else</em> en Erlang.</p>"""

  subunit1_6 = SubunitCreate(
      title="Expresiones avanzadas",
      description="This is the sixth subunit of unit 1.",
      order=6,
      blocks=[
          {"type": "text", "value": info1},
          {"type": "code", "value": code1},
          {"type": "text", "value": info2},
          {"type": "code", "value": code2},
          {"type": "text", "value": info3},
          {"type": "code", "value": code3},
          {"type": "text", "value": info4},
          {"type": "code", "value": code4},
          {"type": "text", "value": info5},
          {"type": "code", "value": code5},
          {"type": "text", "value": info6},
          {"type": "code", "value": code6},
          {"type": "text", "value": info7}
      ],
      unit_id=unit1_id
  )


  subunit2_1_info1 = """    <p>
  En la subunidad anterior aprendimos a usar <strong>Pattern Matching</strong>, <strong>Guardas</strong> y estructuras condicionales como <text-code>if</text-code> para simplificar nuestro código y evitar repeticiones.  
  Ahora vamos a dar un paso más en la programación funcional con Erlang: aprenderemos a trabajar con <strong>Funciones de Orden Superior</strong> y <strong>Funciones Anónimas</strong>.
</p>

<h2>Funciones de Orden Superior</h2>
<p>
  En matemáticas (lambda cálculo) y programación funcional, <strong>una función de orden superior</strong> es aquella que puede:
</p>
<ul>
  <li><strong>Recibir funciones como parámetros.</strong></li>
  <li><strong>Devolver funciones como resultado.</strong></li>
</ul>
<p>
  Esto nos permite crear <strong>abstracciones poderosas</strong> para evitar escribir código repetitivo.
</p>

<h3>Ejemplo básico</h3>"""
  
  subunit2_1_code1 = """-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

start() ->
  io:format("Result of add(1, 2): ~p~n", [add(fun hhfuns:one/0, fun hhfuns:two/0)]).
"""

  subunit2_1_info2 = """  <p><strong>Importante:</strong> Para pasar funciones como argumentos usamos la sintaxis:</p>
  <pre><code class="language-erlang">
fun Module:Function/Arity
  </code></pre>
  <p>
    Si intentamos pasar solo el nombre (<text-code>one</text-code>) o un número (<text-code>1</text-code>), Erlang fallará porque <strong>no son funciones invocables</strong>.
  </p>

  <h3>Eliminando código repetitivo</h3>
  <p>Supongamos que tenemos dos funciones para incrementar o decrementar todos los elementos de una lista:</p>
  <pre><code class="language-erlang">
increment([]) -> [];
increment([H|T]) -> [H+1 | increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1 | decrement(T)].
  </code></pre>
  <p>Son casi idénticas. Podemos <strong>abstraer</strong> la lógica común en una sola función <strong>map/2</strong>:</p>"""

  subunit2_1_code2 = """-module(mapping).
-compile(export_all).

map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

start() ->
  L = [1,2,3,4,5],
  io:format("Increment 1: ~p~n", [map(fun mapping:incr/1, L)]),
  io:format("Decrement 1: ~p~n", [map(fun mapping:decr/1, L)]).
"""
  subunit2_1_info3 = """<p>Ahora, para aplicar cualquier transformación a cada elemento de una lista, solo necesitamos llamar <text-code>map/2</text-code> con la función deseada.</p>

  <h2>Funciones Anónimas (funs)</h2>
  <p>
    Una <strong>función anónima</strong> es una función <strong>sin nombre</strong>, definida en línea. Sirve para evitar tener que declarar y exportar funciones en un módulo solo para usarlas una vez.
  </p>

  <h3>Sintaxis</h3>
  <pre><code class="language-erlang">
fun(Args1) -> Exprs;
    (Args2) -> Exprs
end
  </code></pre>

  <h3>Ejemplo básico usando funciones anónimas con <text-code>map/2</text-code></h3>"""
  
  subunit2_1_code3 = """-module(anonymous).
-export([start/0]).

map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].

start() ->
  L = [1,2,3,4,5],
  io:format("Result: ~p~n", [map(fun(X) -> X + 1 end, L)]),
  io:format("Result: ~p~n", [map(fun(X) -> X - 1 end, L)]).
"""

  subunit2_1_info4 = """  <p>Así nos ahorramos definir <text-code>incr/1</text-code> y <text-code>decr/1</text-code> por separado.</p>

  <h2>Closures: funciones que recuerdan su contexto</h2>
  <p>
    Las funciones anónimas pueden <strong>capturar variables de su entorno</strong> y seguir accediendo a ellas aunque se ejecuten en otro lugar. Esto se llama <strong>closure</strong>.
  </p>"""

  subunit2_1_code4 = """-module(secret).
-compile(export_all).

a() ->
    Secret = "pony",
    fun() -> Secret end.

b(F) ->
    "Password is " ++ F().

start() ->
    io:format("Result: ~p~n", [b(a())])."""
  
  subunit2_1_info5 = """  <p>La variable <text-code>Secret</text-code> sigue viva dentro de la función anónima, incluso fuera de <text-code>a/0</text-code>.</p>

  <h2>Funciones anónimas recursivas</h2>
  <p>
    Desde Erlang 17, podemos dar <strong>un nombre interno</strong> a una función anónima para hacerla recursiva:
  </p>
  <pre><code class="language-erlang">
PrepareAlarm = fun(Room) ->
    io:format("Alarm set in ~s.~n",[Room]),
    fun Loop() ->
        io:format("Alarm tripped in ~s!~n",[Room]),
        timer:sleep(500),
        Loop()
    end
end.
  </code></pre>
<p>Que provocará lo siguiente en caso de ejecutarlo en la shell</p>
<pre><code>
c(recursive).   % compile the module
1&gt; AlarmLoop = recursive:PrepareAlarm("Kitchen").
Alarm set in Kitchen.
#Fun&lt;...&gt;   % This is the Loop function returned
2&gt; AlarmLoop().
Alarm tripped in Kitchen!
Alarm tripped in Kitchen!
...  % continues forever until you break it
  </code></pre>

    <h2>Resumen visual</h2>
  <table>
    <thead>
      <tr>
        <th>Concepto</th>
        <th>Descripción</th>
        <th>Ejemplo</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Función de Orden Superior</td>
        <td>Recibe o devuelve funciones</td>
        <td><text-code>map/2</text-code>, <text-code>filter/2</text-code></td>
      </tr>
      <tr>
        <td>Función Anónima</td>
        <td>Definida en línea, sin nombre</td>
        <td><text-code>fun(X) -&gt; X+1 end</text-code></td>
      </tr>
      <tr>
        <td>Closure</td>
        <td>Función que recuerda variables externas</td>
        <td><text-code>fun() -&gt; Secret end</text-code></td>
      </tr>
      <tr>
        <td>Recursión anónima</td>
        <td>Función anónima con nombre interno</td>
        <td><text-code>fun Loop() -&gt; ... end</text-code></td>
      </tr>
    </tbody>
  </table>

  <h2>Conclusión</h2>
  <ul>
    <li>Eliminamos código repetitivo.</li>
    <li>Creamos abstracciones reutilizables (<text-code>map</text-code>, <text-code>filter</text-code>, <text-code>fold</text-code>).</li>
    <li>Trabajamos de forma más declarativa, enfocándonos en <strong>qué hacer</strong> y no en <strong>cómo hacerlo</strong>.</li>
    <li>Podemos pasar y devolver funciones con facilidad, ¡algo fundamental en programación funcional!</li>
  </ul>
  """



  subunit2_1 = SubunitCreate(
      title="Funciones",
      description="This is the first subunit of unit 2.",
      order=1,
      blocks=[
          {"type": "text", "value": subunit2_1_info1},
          {"type": "code", "value" : subunit2_1_code1},
          {"type": "text", "value" : subunit2_1_info2},
          {"type": "code", "value" : subunit2_1_code2},
          {"type": "text", "value" : subunit2_1_info3},
          {"type": "code", "value" : subunit2_1_code3},
          {"type": "text", "value" : subunit2_1_info4},
          {"type": "code", "value" : subunit2_1_code4},
          {"type": "text", "value" : subunit2_1_info5},
      ],
      unit_id=unit2_id
  )

  subunit2_2_info1 = """    <p>
      La recursión es una de las piedras angulares de Erlang y de la programación funcional en general.<br>
      En Erlang, no existen bucles clásicos como <text-code>for</text-code> o <text-code>while</text-code>. En su lugar, se usan <strong>funciones que se llaman a sí mismas</strong> hasta llegar a una <strong>condición de parada</strong> (<em>base case</em>).
    </p>

    <hr>

    <h2>1. ¿Qué es la recursión?</h2>
    <blockquote>
      <strong>Definición:</strong> Una función recursiva es aquella que se llama a sí misma para resolver un problema más pequeño, hasta alcanzar un caso base donde deja de llamarse.
    </blockquote>

    <p><strong>Ejemplo matemático: factorial</strong></p>
    <p>Matemáticamente:</p>
    <img src="https://learnyousomeerlang.com/static/img/fac.png">

    <p><strong>En Erlang:</strong></p>"""
  
  subunit2_2_code1 = """-module(recursive).
-compile(export_all).

fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N - 1).

start() ->
  io:format("Factorial de 5: ~p~n", [fac(5)]),
  io:format("Factorial de 10: ~p~n", [fac(10)]).
"""

  subunit2_2_info2 = """    <hr>

    <h2>2. Caso base y caso recursivo</h2>
    <ul>
      <li><strong>Caso base:</strong> condición que detiene la recursión.</li>
      <li><strong>Caso recursivo:</strong> llama nuevamente a la función, reduciendo el problema.</li>
    </ul>
    <p>Ejemplo: calcular la longitud de una lista.</p>"""

  subunit2_2_code2 = """-module(list_len).
-compile(export_all).

len([]) -> 0;                        % caso base: lista vacía
len([_|T]) -> 1 + len(T).             % caso recursivo

start() ->
  io:format("Longitud de [1, 2, 3]: ~p~n", [len([1, 2, 3])]),
  io:format("Longitud de []: ~p~n", [len([])]).
"""

  subunit2_2_info3 = """  
  <hr>

    <h2>3. Recursión de cola (<em>Tail Recursion</em>)</h2>
    <p>
      La recursión de cola es una forma de transformar el proceso lineal anterior (crece tanto como hay elementos) en uno iterativo (en realidad no hay ningún crecimiento).<br>
      Esto permite a la <strong>Erlang VM</strong> optimizar el uso de memoria (<em>Tail Call Optimization</em>).
    </p>

    <p><strong>Ejemplo factorial de cola:</strong></p>"""

  subunit2_2_code3 = """-module(tail_recursive).
-compile(export_all).

tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 ->
    tail_fac(N - 1, N * Acc).

start() ->
  io:format("Factorial de 5: ~p~n", [tail_fac(5)]),
  io:format("Factorial de 10: ~p~n", [tail_fac(10)]).
"""

  subunit2_2_info4 = """<p><strong>Ejemplo longitud de lista de cola:</strong></p>"""

  subunit2_2_code4 = """-module(tail_len).
-compile(export_all).

tail_len(L) -> tail_len(L, 0).

tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc + 1).

start() ->
  io:format("Longitud de [1, 2, 3]: ~p~n", [tail_len([1, 2, 3])]),
  io:format("Longitud de []: ~p~n", [tail_len([])]).
"""

  subunit2_2_info5 = """    <hr>

    <h2>4. Ejemplos prácticos</h2>

    <h3>4.1 Duplicar un elemento N veces</h3>"""

  subunit2_2_code5 = """-module(duplicate).
-compile(export_all).

duplicate(0, _) -> [];
duplicate(N, Term) when N > 0 ->
    [Term | duplicate(N - 1, Term)].

start() ->
  io:format("Duplicar 3 veces el elemento 'a': ~p~n", [duplicate(3, a)]),
  io:format("Duplicar 5 veces el elemento 'b': ~p~n", [duplicate(5, b)]).
  """

  subunit2_2_info6 = """<p><strong>Versión de cola:</strong></p>"""

  subunit2_2_code6 = """-module(tail_duplicate).
-compile(export_all).

tail_duplicate(N, Term) -> tail_duplicate(N, Term, []).

tail_duplicate(0, _, List) -> List;
tail_duplicate(N, Term, List) when N > 0 ->
  tail_duplicate(N - 1, Term, [Term | List]).

start() ->
  io:format("Duplicar 3 veces el elemento 'a': ~p~n", [tail_duplicate(3, a)]),
  io:format("Duplicar 5 veces el elemento 'b': ~p~n", [tail_duplicate(5, b)]).
  """

  subunit2_2_info7 = """    <h3>4.2 Revertir una lista</h3>
    <p><strong>Versión simple:</strong></p>"""

  subunit2_2_code7 = """-module(reverse_mod).
-compile(export_all).

reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].

start() ->
  io:format("Revertir [1, 2, 3]: ~p~n", [reverse([1, 2, 3])]),
  io:format("Revertir []: ~p~n", [reverse([])]).
"""

  subunit2_2_info8 = """<p><strong>Versión de cola:</strong></p>"""

  subunit2_2_code8 = """-module(tail_reverse).
-compile(export_all).

tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) -> tail_reverse(T, [H|Acc]).

start() ->
  io:format("Revertir [1, 2, 3]: ~p~n", [tail_reverse([1, 2, 3])]),
  io:format("Revertir []: ~p~n", [tail_reverse([])]).
"""

  subunit2_2_info9 = """<h3>4.3 Sublista de los primeros N elementos</h3>"""

  subunit2_2_code9 = """-module(first).
-compile(export_all).

sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([H|T], N) when N > 0 ->
  [H | sublist(T, N - 1)].

start() ->
  io:format("Sublista de [1, 2, 3, 4, 5] hasta 3: ~p~n", [sublist([1, 2, 3, 4, 5], 3)]),
  io:format("Sublista de [1, 2, 3] hasta 5: ~p~n", [sublist([1, 2, 3], 5)]).
"""

  subunit2_2_info10 = """<p><strong>Versión de cola (corrigiendo el orden):</strong></p>"""

  subunit2_2_code10 = """-module(tail_first).
-compile(export_all).

tail_sublist(L, N) -> tail_sublist(L, N, []).
 
tail_sublist(_, 0, SubList) -> SubList;
tail_sublist([], _, SubList) -> SubList;
tail_sublist([H|T], N, SubList) when N > 0 ->
tail_sublist(T, N-1, [H|SubList]).

start() ->
  io:format("Sublista de [1, 2, 3, 4, 5] hasta 3: ~p~n", [tail_sublist([1, 2, 3, 4, 5], 3)]),
  io:format("Sublista de [1, 2, 3] hasta 5: ~p~n", [tail_sublist([1, 2, 3], 5)]).
"""
  
  subunit2_2_info11 = """<h3>4.4 Zip de dos listas</h3>"""

  subunit2_2_code11 = """-module(zipping).
-compile(export_all).

zip([],_) -> [];
zip(_,[]) -> [];
zip([X|Xs],[Y|Ys]) -> [{X,Y}|zip(Xs,Ys)].
  
start() ->
  io:format("Zip de [1, 2, 3] y [a, b, c]: ~p~n", [zip([1, 2, 3], [a, b, c])]),
  io:format("Zip de [1, 2] y [a, b, c]: ~p~n", [zip([1, 2], [a, b, c])]),
  io:format("Zip de [1, 2, 3] y []: ~p~n", [zip([1, 2, 3], [])]).
"""

  subunit2_2_info12 = """<p><strong>Versión tolerante a longitudes distintas:</strong></p>"""

  subunit2_2_code12 = """-module(lenient_zipping).
-compile(export_all).

lenient_zip([],_) -> [];
lenient_zip(_,[]) -> [];
lenient_zip([X|Xs],[Y|Ys]) -> [{X,Y}|lenient_zip(Xs,Ys)].

start() ->
  io:format("Zip de [1, 2, 3] y [a, b, c]: ~p~n", [lenient_zip([1, 2, 3], [a, b, c])]),
  io:format("Zip de [1, 2] y [a, b, c]: ~p~n", [lenient_zip([1, 2], [a, b, c])]),
  io:format("Zip de [1, 2, 3] y []: ~p~n", [lenient_zip([1, 2, 3], [])]).
"""

  subunit2_2_info13 = """  
        <hr>
    
        <h2>5. Ejemplo avanzado: Quicksort</h2>"""

  subunit2_2_code13 = """-module(quicking).
-compile(export_all).

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
    {Smaller, Larger} = partition(Pivot, Rest, [], []),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) ->
    {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
    if
        H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
        H >  Pivot -> partition(Pivot, T, Smaller, [H|Larger])
    end.
    
start() ->
  L = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5],
  io:format("Lista original: ~p~n", [L]),
  io:format("Lista ordenada: ~p~n", [quicksort(L)]).
"""
  
  subunit2_2_info14 = """   <hr>

    <h2>7. Patrones para pensar recursivamente</h2>
    <ol>
      <li><strong>Identifica el caso base</strong> (cuándo detenerte).</li>
      <li><strong>Reduce el problema</strong> hacia el caso base en cada llamada.</li>
      <li><strong>Usa acumuladores</strong> para convertir en recursión de cola y optimizar memoria.</li>
      <li><strong>Aprovecha el pattern matching</strong> para simplificar código.</li>
      <li><strong>Evita reinventar funciones</strong>: usa funciones de <text-code>lists</text-code> cuando sea posible.</li>
    </ol>

    <hr>

    <h2>8. Conclusión</h2>
    <ul>
      <li>Erlang no tiene bucles clásicos: <strong>recursión es la herramienta principal</strong>.</li>
      <li><strong>Recursión de cola</strong> = más eficiente en memoria.</li>
      <li>Es un estilo declarativo: describimos <strong>qué hacer en cada caso</strong>, no los pasos imperativos.</li>
      <li>Funciona no solo con listas, sino con cualquier estructura recursiva.</li>
    </ul>"""
  
  
  subunit2_2 = SubunitCreate(
      title="Recursión",
      description="This is the second subunit of unit 2.",
      order=2,
      blocks=[
          {"type" : "text", "value" : subunit2_2_info1},
          {"type" : "code", "value" : subunit2_2_code1},
          {"type" : "text", "value" : subunit2_2_info2},
          {"type" : "code", "value" : subunit2_2_code2},
          {"type" : "text", "value" : subunit2_2_info3},
          {"type" : "code", "value" : subunit2_2_code3},
          {"type" : "text", "value" : subunit2_2_info4},
          {"type" : "code", "value" : subunit2_2_code4},
          {"type" : "text", "value" : subunit2_2_info5},
          {"type" : "code", "value" : subunit2_2_code5},
          {"type" : "text", "value" : subunit2_2_info6},
          {"type" : "code", "value" : subunit2_2_code6},
          {"type" : "text", "value" : subunit2_2_info7},
          {"type" : "code", "value" : subunit2_2_code7},
          {"type" : "text", "value" : subunit2_2_info8},
          {"type" : "code", "value" : subunit2_2_code8},
          {"type" : "text", "value" : subunit2_2_info9},
          {"type" : "code", "value" : subunit2_2_code9},
          {"type" : "text", "value" : subunit2_2_info10},
          {"type" : "code", "value" : subunit2_2_code10},
          {"type" : "text", "value" : subunit2_2_info11},
          {"type" : "code", "value" : subunit2_2_code11},
          {"type" : "text", "value" : subunit2_2_info12},
          {"type" : "code", "value" : subunit2_2_code12},
          {"type" : "text", "value" : subunit2_2_info13},
          {"type" : "code", "value" : subunit2_2_code13},
          {"type" : "text", "value" : subunit2_2_info14},
      ],
      unit_id=unit2_id
  )

  subunit2_3_info1 = """<p>Este tutorial cubre tres tipos de estructuras de datos comunes en Erlang: <strong>records</strong>, <strong>almacenamiento clave/valor</strong> y <strong>conjuntos</strong>.  
Cada sección incluye ejemplos prácticos y notas importantes para ayudarte a comprender y usar estas estructuras de forma efectiva.</p>

<hr>

<h2>1. Records</h2>
<p>Los <strong>records</strong> en Erlang son similares a las <strong>structs</strong> en C: permiten almacenar varios campos con nombre en una sola estructura.<br>
Internamente, son <em>azúcar sintáctico sobre tuplas</em>, lo que significa que el compilador los traduce a tuplas, pero tú trabajas con nombres de campo.</p>

<h3>Declaración y creación de un record</h3>"""

  subunit2_3_code1 = """-module(records).
-compile(export_all).

-record(robot, {
    name,
    type = industrial,     % valor por defecto
    hobbies,
    details = []           % valor por defecto
}).

first_robot() ->  #robot{
        name = "Mechatron",
        type = handmade,
        details = ["Moved by a small man inside"]
    }.

start() ->
  io:format("First robot: ~p~n", [first_robot()]).
"""

  subunit2_3_info2 = """
  <p><strong>Nota:</strong> Si un campo no tiene valor por defecto y no se asigna, Erlang lo establece en <text-code>undefined</text-code>.</p>
  <h3>Acceder a campos</h3>
  """

  subunit2_3_code2 = """-module(records).
-compile(export_all).

-record(robot, {
    name,
    type = industrial, 
    hobbies,
    details = []         
}).

start() ->
  Crusher = #robot{name="Crusher", hobbies=["Crushing","petting cats"]},
  io:format("Second robot: ~p~n", [Crusher#robot.hobbies]).
"""

  subunit2_3_info3 = """<h3>Records anidados</h3>"""

  subunit2_3_code3 = """-module(records).
-compile(export_all).

-record(robot, {
    name,
    type = industrial, 
    hobbies,
    details = []         
}).

start() -> 
  NestedBot = #robot{details=#robot{name="erNest"}},
  io:format("Nested robot: ~p~n", [NestedBot#robot.details#robot.name]).
"""

  subunit2_3_info4 = """<h3>Pattern Matching en records</h3>"""

  subunit2_3_code4 = """-module(records).
-compile(export_all).
  
-record(user, {id, name, group, age}).

admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed".

adult_section(U = #user{}) when U#user.age >= 18 ->
    allowed;
adult_section(_) ->
    forbidden.

start() ->
  User1 = #user{id=1, name="Alice", group=admin, age=30},
  User2 = #user{id=2, name="Bob", group=user, age=25},
  io:format("User1: ~p~n", [User1]),
  io:format("User2: ~p~n", [User2]).
"""

  subunit2_3_info5 = """<h3>Actualizar un record</h3>"""

  subunit2_3_code5 = """-module(records).
-compile(export_all).

-record(robot, {
    name,
    type = industrial, 
    hobbies,
    details = []         
}).

repairman(Rob) ->
    Details = Rob#robot.details,
    Rob#robot{details=["Repaired by repairman" | Details]}.

start() ->
  Rob = #robot{name="Robo", hobbies=["Dancing", "Singing"]},
  io:format("Robot: ~p~n", [repairman(Rob)]).
"""

  subunit2_3_info6 = """<hr>

<h2>2. Almacenamiento Clave/Valor</h2>
<p>En Erlang, hay varias maneras de almacenar datos asociados a claves.<br>
La elección depende del tamaño de los datos y el uso que se les dará.</p>

<h3>2.1. Proplists</h3>
<p>Lista de tuplas <text-code>{Clave, Valor}</text-code>.<br>
Estructura flexible, pero no estricta.</p>"""

  subunit2_3_code6 = """-module(propping).
-compile(export_all).

start() ->
  Config = [{user, "Alice"}, {theme, dark}],
  io:format("~p~n", [proplists:get_value(user, Config)]).
"""

  subunit2_3_info7 = """<p>Módulo: <a href="https://www.erlang.org/doc/man/proplists.html">proplists</a></p>
<ul>
  <li><text-code>get_value/2</text-code></li>
  <li><text-code>lookup/2</text-code></li>
  <li><text-code>delete/2</text-code></li>
</ul>

<h3>2.2. Orddict</h3>
<p>Versión ordenada de proplists, con claves únicas.<br>
Eficiente hasta ~75 elementos.</p>"""

  subunit2_3_code7 = """-module(orddicting).
-compile(export_all).

start() ->
  D = orddict:store(user, "Alice", orddict:new()),
  io:format("~p~n", [orddict:find(user, D)]).
"""
  subunit2_3_info8 = """<h3>2.3. Dict</h3>
<p>Escala mejor que orddict para más datos. Misma interfaz.</p>"""
  
  subunit2_3_code8 = """-module(dicting).
-compile(export_all).

start() ->
  D = dict:store(user, "Alice", dict:new()),
  io:format("~p~n", [dict:find(user, D)]).
"""
  subunit2_3_info9 = """<p><strong>Elección general:</strong></p>
<ul>
  <li>Datos pequeños → <text-code>proplists</text-code> o <text-code>orddict</text-code></li>
  <li>Datos más grandes → <text-code>dict</text-code></li>
</ul>

<hr>

<h2>3. Conjuntos</h2>
<p>Un <strong>set</strong> es un grupo de elementos <strong>únicos</strong> con operaciones como unión, intersección y diferencia.</p>

<h3>3.1. Ordsets</h3>
<ul>
  <li>Lista ordenada.</li>
  <li>Simples, buenas para sets pequeños.</li>
</ul>"""

  subunit2_3_code9 = """-module(ordsetting).
-compile(export_all).

start() ->
  S = ordsets:add_element(3, ordsets:new()),
  io:format("~p~n", [ordsets:is_element(3, S)]).
"""

  subunit2_3_info10 = """<h3>3.2. Sets</h3>
<ul>
  <li>Basados en estructura tipo <text-code>dict</text-code>.</li>
  <li>Usan <text-code>=:=</text-code> para comparar (más estrictos).</li>
</ul>"""

  subunit2_3_code10 = """-module(setting).
-compile(export_all).

start() ->
  S = sets:add_element(3, sets:new()),
  io:format("~p~n", [sets:is_element(3, S)]).
"""

  subunit2_3_info11 = """<hr>

<h2>Resumen rápido</h2>
<table>
  <thead>
    <tr>
      <th>Necesidad</th>
      <th>Estructura recomendada</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Campos nombrados</td>
      <td><strong>Records</strong></td>
    </tr>
    <tr>
      <td>Pocos pares clave/valor</td>
      <td><strong>Proplists</strong></td>
    </tr>
    <tr>
      <td>Clave/valor <strong>ordenado</strong></td>
      <td><strong>Orddict</strong></td>
    </tr>
    <tr>
      <td>Clave/valor más grande</td>
      <td><strong>Dict</strong></td>
    </tr>
    <tr>
      <td>Set pequeño y simple</td>
      <td><strong>Ordsets</strong></td>
    </tr>
    <tr>
      <td>Set grande (comparación estricta)</td>
      <td><strong>Sets</strong></td>
    </tr>
  </tbody>
</table>"""

  subunit2_3 = SubunitCreate(
      title="Estructuras de datos",
      description="This is the third subunit of unit 2.",
      order=3,
      blocks=[
          {"type": "text", "value": subunit2_3_info1},
          {"type": "code", "value": subunit2_3_code1},
          {"type": "text", "value": subunit2_3_info2},
          {"type": "code", "value": subunit2_3_code2},
          {"type": "text", "value": subunit2_3_info3},
          {"type": "code", "value": subunit2_3_code3},
          {"type": "text", "value": subunit2_3_info4},
          {"type": "code", "value": subunit2_3_code4},
          {"type": "text", "value": subunit2_3_info5},
          {"type": "code", "value": subunit2_3_code5},
          {"type": "text", "value": subunit2_3_info6},
          {"type": "code", "value": subunit2_3_code6},
          {"type": "text", "value": subunit2_3_info7},
          {"type": "code", "value": subunit2_3_code7},
          {"type": "text", "value": subunit2_3_info8},
          {"type": "code", "value": subunit2_3_code8},
          {"type": "text", "value": subunit2_3_info9},
          {"type": "code", "value": subunit2_3_code9},
          {"type": "text", "value": subunit2_3_info10},
          {"type": "code", "value": subunit2_3_code10},
          {"type": "text", "value": subunit2_3_info11},
      ],
      unit_id=unit2_id
  )

  subunit2_4_info1 = """
  <p>
    Erlang favorece la robustez: a veces, lo correcto es <em>dejar que un proceso falle</em> y que otro lo supervise.
    En este tutorial nos enfocamos en el manejo de errores en el código <strong>funcional y secuencial</strong>,
    base necesaria antes de pasar a la parte concurrente y de supervisión.
  </p>

  <hr/>

  <h2>1. Tipos de errores</h2>

  <h3>1.1 Errores de compilación</h3>
  <p>Se detectan antes de ejecutar. Suelen ser de sintaxis, nombres o aridades incorrectas.</p>

  <table>
    <thead>
      <tr>
        <th>Mensaje</th>
        <th>Causa</th>
        <th>Solución</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td><text-code>Module name 'madule' does not match file name 'module'</text-code></td>
        <td>Nombre en <text-code>-module</text-code> no coincide con el archivo.</td>
        <td>Corregir el nombre del módulo o del archivo.</td>
      </tr>
      <tr>
        <td><text-code>function some_function/1 undefined</text-code></td>
        <td>Nombre o aridad incorrecta; o fallo de compilación previo.</td>
        <td>Verificar <text-code>-export</text-code>, nombre y número de argumentos.</td>
      </tr>
      <tr>
        <td><text-code>syntax error before: 'X'</text-code></td>
        <td>Paréntesis/tuplas sin cerrar; terminación incorrecta.</td>
        <td>Revisar llaves, comas y puntos finales.</td>
      </tr>
      <tr>
        <td><text-code>Warning: variable 'Var' is unused</text-code></td>
        <td>Variable no utilizada.</td>
        <td>Eliminarla o renombrarla a <text-code>_Var</text-code>.</td>
      </tr>
      <tr>
        <td><text-code>Warning: a term is constructed, but never used</text-code></td>
        <td>Construcción inútil (lista/tupla/fun) no usada.</td>
        <td>Asignar/retornar el valor o eliminarlo.</td>
      </tr>
      <tr>
        <td><text-code>head mismatch</text-code></td>
        <td>Cláusulas de una misma función con aridades distintas.</td>
        <td>No intercalar definiciones ni mezclar aridades.</td>
      </tr>
      <tr>
        <td><text-code>this clause cannot match...</text-code></td>
        <td>Cláusula inalcanzable tras una de “captura-todo”.</td>
        <td>Reordenar o ajustar patrones.</td>
      </tr>
      <tr>
        <td><text-code>variable 'A' unsafe in 'case'</text-code></td>
        <td>Variable usada fuera del ámbito del <text-code>case</text-code>.</td>
        <td>Asigna el <text-code>case</text-code> a una variable.</td>
      </tr>
    </tbody>
  </table>

  <p>Consejo: corrige en el orden en que aparecen para evitar “cascadas” de errores engañosos.</p>

  <h3>1.2 Errores lógicos</h3>
  <p>El programa “funciona”, pero hace lo incorrecto. Se detectan con pruebas y análisis estático (EUnit, TypEr/Dialyzer).</p>

  <pre><code>% Ejemplo: rama no considerada (no falla, pero no hace nada útil)
  if X > 10 -> ok end.
</code></pre>

<h3>1.3 Errores en tiempo de ejecución</h3>
  <p>Frenan la ejecución. Algunos comunes:</p>
  <table>
    <thead>
      <tr>
        <th>Error</th>
        <th>Causa</th>
        <th>Ejemplo</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td><text-code>function_clause</text-code></td>
        <td>Ninguna cláusula coincide.</td>
        <td><text-code>lists:sort(fffff).</text-code></td>
      </tr>
      <tr>
        <td><text-code>case_clause</text-code></td>
        <td><text-code>case</text-code> sin patrón válido.</td>
        <td>Falta cláusula por defecto.</td>
      </tr>
      <tr>
        <td><text-code>badmatch</text-code></td>
        <td>Fallo en pattern matching.</td>
        <td><text-code>[X,Y] = {4,5}.</text-code></td>
      </tr>
      <tr>
        <td><text-code>badarg</text-code></td>
        <td>Argumento inválido.</td>
        <td><text-code>binary_to_list("ya es lista").</text-code></td>
      </tr>
      <tr>
        <td><text-code>undef</text-code></td>
        <td>Función inexistente/no exportada/fuera del path.</td>
        <td><text-code>lists:random/1</text-code> (no existe).</td>
      </tr>
      <tr>
        <td><text-code>badarith</text-code></td>
        <td>Operación aritmética inválida.</td>
        <td><text-code>5 + llama.</text-code></td>
      </tr>
      <tr>
        <td><text-code>badfun</text-code></td>
        <td>Variable no es una función.</td>
        <td><text-code>F = atom, F().</text-code></td>
      </tr>
      <tr>
        <td><text-code>badarity</text-code></td>
        <td>Aridad incorrecta al invocar.</td>
        <td><text-code>F(a,b)</text-code> cuando espera 1.</td>
      </tr>
      <tr>
        <td><text-code>system_limit</text-code></td>
        <td>Límites de VM (procesos, átomos, etc.).</td>
        <td>Consultar “Erlang Efficiency Guide”.</td>
      </tr>
    </tbody>
  </table>

  <hr />

  <h2>2. Tipos de excepciones en Erlang</h2>
  <table>
    <thead>
      <tr>
        <th>Tipo</th>
        <th>Cómo se lanza</th>
        <th>Uso</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td><strong>Error</strong></td>
        <td><text-code>erlang:error(Reason)</text-code></td>
        <td>Situaciones graves que el llamador no puede manejar. Incluye <em>stack trace</em>.</td>
      </tr>
      <tr>
        <td><strong>Exit</strong></td>
        <td><text-code>exit(Reason)</text-code></td>
        <td>Termina el proceso actual (o externo con <text-code>exit(Pid, Reason)</text-code>). Sin <em>stack trace</em>.</td>
      </tr>
      <tr>
        <td><strong>Throw</strong></td>
        <td><text-code>throw(Value)</text-code></td>
        <td>Control de flujo que el llamador debe capturar. Útil para <em>non-local returns</em>.</td>
      </tr>
    </tbody>
  </table>
  """

  subunit2_4_code1_1 = """-module(exceptions).
-export([start/0]).

start() ->
  erlang:error(custom_error).
  """

  subunit2_4_code1_2 = """-module(throwing).
-export([start/0]).

start() ->
  throw(not_found).
  """ 

  subunit2_4_info2 = """<hr />

  <h2>4. Manejo con <text-code>try ... catch</text-code></h2>
  <p>Estructura general:</p>
  <pre><code>try Expresion of
      Patron1 -> Resultado1;
      Patron2 -> Resultado2
  catch
      Tipo:Excepcion -> Manejo
  end.</code></pre>

  <p>Donde <span class="kbd">Tipo</span> puede ser <text-code>error</text-code>, <text-code>exit</text-code>, <text-code>throw</text-code> o se omite (equivale a <text-code>throw</text-code>).</p>

  <h4>Ejemplo básico</h4>
  """

  subunit2_4_code2 = """-module(exceptions).
-compile(export_all).
 
  throws(F) ->
    try F() of
      _ -> ok
    catch
      Throw -> {throw, caught, Throw}
    end.

  start() ->
    io:format("~p~n", [throws(fun() -> throw(not_found) end)]).
  """

  subunit2_4_info3 = """<hr/>

  <h2>5. Múltiples excepciones</h2>"""

  subunit2_4_code3 = """-module(knight).
-compile(export_all).

black_knight(Attack) when is_function(Attack, 0) ->
    try Attack() of
        _ -> "None shall pass."
    catch
        throw:slice   -> "It is but a scratch.";
        error:cut_arm -> "I've had worse.";
        exit:cut_leg  -> "Come on you pansy!";
        _:_           -> "Just a flesh wound."
    end.
start() ->
  io:format("~p~n", [black_knight(fun() -> throw(slice) end)]),
  io:format("~p~n", [black_knight(fun() -> erlang:error(cut_arm) end)]),
  io:format("~p~n", [black_knight(fun() -> exit(cut_leg) end)]).
  """

  subunit2_4_info4 = """<hr/>

  <h2>6. Bloque <text-code>after</text-code> (equivalente a <em>finally</em>)</h2>
  <p>Siempre se ejecuta, ocurra o no una excepción. Útil para cerrar archivos, liberar recursos, etc.</p>"""

  subunit2_4_code4 = """-module(closing).
-export([start/0]).

start() ->
  try
    io:format("Procesando...~n"),
    timer:sleep(500),
    erlang:error(something_went_wrong)
  catch
      error:Reason -> io:format("Ocurrió un error: ~p~n", [Reason])
  after
      io:format("Liberando recursos...~n")
  end.
  """

  subunit2_4_info5 = """<p>Nota: <text-code>after</text-code> no devuelve valor; úsalo para efectos secundarios.</p>
  <hr />
  <h2>7. Uso de <text-code>catch</text-code> (forma antigua)</h2>

  <pre><code>catch throw(whoa).     %% whoa
  catch exit(die).       %% {'EXIT',die}
  catch 1/0.             %% {'EXIT',{badarith,...}}
  catch 2+2.             %% 4
  </code></pre>

  <p>Desventajas:</p>
  <ul>
    <li>Precedencia incómoda: se suele requerir <text-code>(catch Expr)</text-code>.</li>
    <li>Puede confundir valores con excepciones y no distingue bien <text-code>exit</text-code> vs <text-code>error</text-code>.</li>
    <li>No sabes si un valor provino de un retorno normal o de un <text-code>throw</text-code>.</li>
  </ul>

  <p><strong>Recomendación:</strong> preferir <text-code>try ... catch</text-code>.</p>

  <hr />

  <h2>8. Buenas prácticas</h2>
  <ul>
    <li>Usa <text-code>throw</text-code> solo para flujos controlados y, de ser posible, encapsulado en un mismo módulo.</li>
    <li>Prefiere <text-code>try ... catch</text-code> sobre <text-code>catch</text-code>.</li>
    <li>Evita el <em>catch-all</em> (<text-code>_:_</text-code>) si no vas a manejar algo de forma específica.</li>
    <li>Ojo: el bloque protegido de <text-code>try</text-code> no es <em>tail-recursive</em>. Pon recursión entre <text-code>of</text-code> y <text-code>catch</text-code> si corresponde.</li>
    <li>Abraza la filosofía “<em>let it crash</em>” cuando convenga y apóyate en supervisores (en la parte concurrente).</li>
  </ul>
  """



  subunit2_4 = SubunitCreate(
      title="Errores y Excepciones",
      description="This is the first subunit of unit 3.",
      order=4,
      blocks=[
          {"type": "text", "value": subunit2_4_info1},
          {"type": "code", "value": subunit2_4_code1_1},
          {"type": "code", "value": subunit2_4_code1_2},
          {"type": "text", "value": subunit2_4_info2},
          {"type": "code", "value": subunit2_4_code2},
          {"type": "text", "value": subunit2_4_info3},
          {"type": "code", "value": subunit2_4_code3},
          {"type": "text", "value": subunit2_4_info4},
          {"type": "code", "value": subunit2_4_code4},
          {"type": "text", "value": subunit2_4_info5},
      ],
      unit_id=unit2_id
  )

  subunit3_1_info1 = """<h2>1. Concurrencia vs Paralelismo</h2>
  <p>En Erlang:</p>
  <ul>
    <li><strong>Concurrencia</strong>: múltiples procesos (actores) ejecutándose de forma independiente, aunque no necesariamente al mismo tiempo.</li>
    <li><strong>Paralelismo</strong>: procesos ejecutándose exactamente al mismo tiempo (distintos núcleos).</li>
  </ul>
  <h2>2. Filosofía de Erlang</h2>
  <p>Basada en el <strong>modelo de actores</strong>:</p>
  <ul>
    <li>Procesos aislados (sin memoria compartida).</li>
    <li>Comunicación por <strong>paso de mensajes</strong>.</li>
    <li>Procesos ligeros, rápidos de crear y destruir.</li>
    <li>En caso de fallo: <em>let it crash</em> y reiniciar.</li>
  </ul>

  <h2>3. Procesos en Erlang</h2>
  <p>Un proceso es una función con un <strong>PID</strong> y una <em>mailbox</em>.</p>
  <h3>Creación de procesos</h3>"""

  subunit3_1_code1 = """-module(spawning).
-compile(export_all).

start() ->
  F = fun() -> io:format("Result: ~p~n", [2 + 2]) end,
  io:format("Spawn: ~p~n", [spawn(F)]).
  """

  subunit3_1_info2 = """<h2>4. Mensajería</h2>
  <h3>Enviar mensajes</h3>
  <pre><code>1&gt; self() ! hello.
  hello</code></pre>
  <h3>Recibir mensajes</h3>
  <pre><code>receive
    {From, hello} ->
      io:format("Hello Back~n");
    _ ->
      io:format("Unknown message~n")
    end.
  </code></pre>
  <h2>5. Ejemplo: Proceso "delfín"</h2>
  """

  subunit3_1_code2 = """-module(dolphins).
-compile(export_all).

dolphin1() ->
    receive
        do_a_flip ->
            io:format("How about no?~n");
        fish ->
            io:format("So long and thanks for all the fish!~n");
        _ ->
            io:format("Heh, somos más listos que ustedes humanos.~n")
    end.
  
start() ->
    Dolphin = spawn(dolphins, dolphin1, []),
    timer:sleep(100),
    Dolphin ! "oh, hello dolphin!",
    Dolphin ! fish.
  """

  subunit3_1_info3 = """  <p>Aquí introducimos una nueva forma de crear procesos usando <text-code>spawn/3</text-code>.
    En lugar de recibir una única función, <text-code>spawn/3</text-code> toma como argumentos el módulo, la función y
    sus argumentos.
    Una vez que la función está en ejecución, sucede lo siguiente:</p>
  <ul>
    <li>La función llega a la sentencia <text-code>receive</text-code>. Dado que el <em>mailbox</em> del proceso está
      vacío, nuestro delfín espera hasta recibir un mensaje.</li>
    <li>Se recibe el mensaje <text-code>"oh, hello dolphin!"</text-code>. La función intenta hacer <em>pattern
        matching</em> contra <text-code>do_a_flip</text-code>. Esto falla, por lo que se prueba con el patrón
      <text-code>fish</text-code>, que también falla. Finalmente, el mensaje coincide con la cláusula comodín
      (<text-code>_</text-code>).</li>
    <li>El proceso muestra el mensaje <em>"Heh, somos más listos que ustedes humanos."</em>.</li>
    <li>Es importante notar que, si el primer mensaje que enviamos funcionó, el segundo no provocó ninguna reacción en
      el proceso <text-code>&lt;0.40.0&gt;</text-code>. Esto se debe a que, una vez que la función imprimió el mensaje,
      terminó, y con ella el proceso. Es necesario reiniciar el delfín para que vuelva a responder.</li>
  </ul>
  <h2>6. Respondiendo al remitente</h2>"""

  subunit3_1_code3 = """-module(dolphins).
-compile(export_all).

  dolphin2() ->
      receive
          {From, do_a_flip} ->
              From ! "How about no?";
          {From, fish} ->
              From ! "So long and thanks for all the fish!";
          _ ->
              io:format("Heh, somos más listos que ustedes humanos.~n")
      end.

start() ->
    Dolphin2 = spawn(dolphins, dolphin2, []),
    timer:sleep(100),
    Dolphin2 ! {self(), do_a_flip},
    receive
        Msg -> io:format("~p~n",[Msg])
    end."""

  subunit3_1_info4 = """<p>Como puede ver, en lugar de aceptar <text-code>do_a_flip</text-code> y buscar mensajes, ahora requerimos una
    variable <text-code>From</text-code>. Ahí irá el identificador del proceso.</p>
  <p>Aqui una prueba desde la Shell: </p>

  <pre><code>1&gt; c(dolphins).
{ok,dolphins}
2&gt; Dolphin2 = spawn(dolphins, dolphin2, []).
&lt;0.65.0&gt;
3&gt; Dolphin2 ! {self(), do_a_flip}.
{&lt;0.32.0&gt;,do_a_flip}
4&gt; flush().
Shell got "How about no?"
ok
</code></pre>

  <h2>7. Procesos persistentes</h2>

  <p>Los procesos en Erlang son ligeros y se pueden crear y destruir fácilmente. Sin embargo, a veces es necesario
    mantener un proceso en ejecución durante un período prolongado. Esto se puede lograr utilizando un bucle de
    recepción que permita al proceso manejar múltiples mensajes a lo largo del tiempo.</p>
  <p>Veamos un ejemplo de un proceso persistente:</p>"""

  subunit3_1_code4 = """-module(dolphins).
-compile(export_all).

dolphin3() ->
      receive
          {From, do_a_flip} ->
              From ! "How about no?",
              dolphin3();
          {From, fish} ->
              From ! "So long and thanks for all the fish!";
          _ ->
              io:format("Heh, somos más listos que ustedes humanos.~n"),
              dolphin3()
      end.

start() ->
    Dolphin3 = spawn(dolphins, dolphin3, []),
    timer:sleep(100),
    Dolphin3 ! Dolphin3 ! {self(), do_a_flip},
    receive
        Msg -> io:format("~p~n",[Msg])
    end,
    Dolphin3 ! {self(), unknown_message},
    receive
        Msg -> io:format("~p~n",[Msg])
    end."""
  
  subunit3_1_info5 = """
  <p> Ahora desde la shell:</p>
  <pre><code>
1&gt; Dolphin3 = spawn(dolphins, dolphin3, []).
&lt;0.75.0&gt;

2&gt; Dolphin3 ! Dolphin3 ! {self(), do_a_flip}.
{&lt;0.32.0&gt;,do_a_flip}

3&gt; flush().
Shell got "How about no?"
Shell got "How about no?"
ok

4&gt; Dolphin3 ! {self(), unknown_message}.    
Heh, we're smarter than you humans.
{&lt;0.32.0&gt;,unknown_message}

5&gt; Dolphin3 ! Dolphin3 ! {self(), fish}.
{&lt;0.32.0&gt;,fish}

6&gt; flush().
Shell got "So long and thanks for all the fish!"
ok
</code></pre>
  <h2>8. Resumen de primitivas</h2>
  <table>
    <thead>
      <tr>
        <th>Acción</th>
        <th>Sintaxis</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Crear proceso</td>
        <td><text-code>spawn(Fun)</text-code> / <text-code>spawn(Mod,Fun,Args)</text-code></td>
      </tr>
      <tr>
        <td>Obtener PID</td>
        <td><text-code>self()</text-code></td>
      </tr>
      <tr>
        <td>Enviar mensaje</td>
        <td><text-code>Pid ! Mensaje</text-code></td>
      </tr>
      <tr>
        <td>Recibir mensaje</td>
        <td><text-code>receive ... end</text-code></td>
      </tr>
      <tr>
        <td>Vaciar mailbox (shell)</td>
        <td><text-code>flush()</text-code></td>
      </tr>
    </tbody>
  </table>
  """

  subunit3_1 = SubunitCreate(
      title="Procesos y Concurrencia",
      description="This is the first subunit of unit 3.",
      order=1,
      blocks=[
          {"type": "text", "value": subunit3_1_info1},
          {"type": "code", "value": subunit3_1_code1},
          {"type": "text", "value": subunit3_1_info2},
          {"type": "text", "value": subunit3_1_code2},
          {"type": "text", "value": subunit3_1_info3},
          {"type": "code", "value": subunit3_1_code3},
          {"type": "text", "value": subunit3_1_info4},
          {"type": "code", "value": subunit3_1_code4},
          {"type": "text", "value": subunit3_1_info5},
      ],
      unit_id=unit3_id
  )

  subunit3_2_info1 = """<h2>Comunicación entre procesos en Erlang</h2>

<p>En Erlang, los procesos pueden comunicarse enviando y recibiendo mensajes. Vamos a ver cómo hacerlo paso a paso.</p>

<h3>1. Crear un proceso con estado</h3>

<p>Primero, vamos a crear un módulo <text-code>kitchen.erl</text-code> que represente un frigorífico. El proceso podrá almacenar y tomar comida:</p>
"""

  subunit3_2_code1 = """-module(kitchen).
-compile(export_all).

fridge(FoodList) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge([Food|FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
        true ->
          From ! {self(), {ok, Food}},
          fridge(lists:delete(Food, FoodList));
        false ->
          From ! {self(), not_found},
          fridge(FoodList)
      end;
    terminate ->
      ok
  end.

start() ->
    Pid = spawn(kitchen, fridge, [[baking_soda]]),

    %% Store bacon
    Pid ! {self(), {store, bacon}},
    receive 
        {Pid, ok} -> io:format("Stored bacon~n")
    end,

    %% Take bacon
    Pid ! {self(), {take, bacon}},
    receive 
        {Pid, {ok, bacon}} -> io:format("Got bacon~n");
        {Pid, not_found} -> io:format("Bacon not found~n")
    end."""
  
  subunit3_2_info2 = """
<p>Este proceso mantiene el estado <text-code>FoodList</text-code> usando recursión, lo que permite almacenar y recuperar comida.</p>

<h3>2. Funciones de interfaz</h3>

<p>Para simplificar el envío y recepción de mensajes, podemos crear funciones <text-code>store/2</text-code> y <text-code>take/2</text-code>:</p>

<pre><code>store(Pid, Food) -&gt;
    Pid ! {self(), {store, Food}},
    receive
        {Pid, Msg} -&gt; Msg
    end.

take(Pid, Food) -&gt;
    Pid ! {self(), {take, Food}},
    receive
        {Pid, Msg} -&gt; Msg
    end.
</code></pre>

<p>Esto oculta la complejidad de los mensajes y permite interactuar con el proceso de manera más limpia. Ahora como es mas limpiar podemos interactuar con el frigorífico de forma más sencilla:</p>

<pre><code>1&gt; c(kitchen).
{ok,kitchen}
2&gt; f().
ok
3&gt; Pid = spawn(kitchen, fridge, [[baking_soda]]).
&lt;0.73.0&gt;
4&gt; kitchen:store(Pid, water).
ok
5&gt; kitchen:take(Pid, water).
{ok,water}
6&gt; kitchen:take(Pid, juice).
not_found</code></pre>

<p>Ejemplo en un modulo: </p>
"""
  subunit3_2_code2 = """-module(kitchen).
-compile(export_all).

fridge(FoodList) ->
    receive
        {From, {store, Food}} ->
            From ! {self(), ok},
            fridge([Food|FoodList]);
        {From, {take, Food}} ->
            case lists:member(Food, FoodList) of
                true ->
                    From ! {self(), {ok, Food}},
                    fridge(lists:delete(Food, FoodList));
                false ->
                    From ! {self(), not_found},
                    fridge(FoodList)
            end;
        terminate ->
            ok
    end.

store(Pid, Food) ->
    Pid ! {self(), {store, Food}},
    receive
        {Pid, Msg} -> Msg
    end.

take(Pid, Food) ->
    Pid ! {self(), {take, Food}},
    receive
        {Pid, Msg} -> Msg
    end.

start() ->
    Pid = spawn(kitchen, fridge, [[rhubarb, dog, hotdog]]),
    io:format("Fridge started with initial food: ~p~n", [[rhubarb, dog, hotdog]]),
    io:format("Taking food from fridge: ~p~n", [kitchen:take(Pid, dog)]),
    io:format("Taking again: ~p~n", [kitchen:take(Pid, dog)])."""

  subunit3_2_info3 = """<h3>3. Manejo de timeouts</h3>

<p>Para evitar que el shell se congele al enviar mensajes a procesos inexistentes, podemos usar <text-code>receive ... after</text-code>:</p>

<pre><code>
store2(Pid, Food) -&gt;
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -&gt; Msg
  after 3000 -&gt;
    timeout
  end.

take2(Pid, Food) -&gt;
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -&gt; Msg
  after 3000 -&gt;
    timeout
  end.
</code></pre>

<p>Ahora, si el proceso no responde en 3 segundos, se devuelve <text-code>timeout</text-code> como podemos ver en este ejemplo.</p>

<pre><code>7&gt; kitchen:take(pid(0,250,0), dog).
%Shell congelada -&gt; Presionamos ^G
User switch command
--&gt; k
--&gt; s
--&gt; c
Eshell V5.7.5  (abort with ^G)
1&gt; c(kitchen).
{ok,kitchen}
2&gt; kitchen:take2(pid(0,250,0), dog).
timeout
</code></pre>

<h3>4. Recepción selectiva</h3>

<p>Se pueden priorizar ciertos mensajes usando recepciones selectivas, esto permite procesar primero los mensajes más importantes:</p>"""

  subunit3_2_code3 = """-module(multiproc).
-compile(export_all).

important() ->
    receive
        {Priority, Message} when Priority > 10 ->
            [Message | important()]
    after 0 ->
        normal()
    end.

normal() ->
    receive
        {_, Message} -> [Message | normal()]
    after 0 ->
        []
    end.

start() ->
    self() ! {15, high},
    self() ! {7, low},
    self() ! {1, low},
    self() ! {17, high},
    io:format("~p~n", [important()])."""
  
  subunit3_2 = SubunitCreate(
      title="Comunicación entre procesos",
      description="This is the second subunit of unit 3.",
      order=2,
      blocks=[
          {"type": "text", "value": subunit3_2_info1},
          {"type": "code", "value": subunit3_2_code1},
          {"type": "text", "value": subunit3_2_info2},
          {"type": "code", "value": subunit3_2_code2},
          {"type": "text", "value": subunit3_2_info3},
          {"type": "code", "value": subunit3_2_code3}
      ],
      unit_id=unit3_id
  )

  subunit3_3_info1 = """  <h2>Enlaces entre Procesos (Links)</h2>
  <p>Un <strong>link</strong> es una relación entre dos procesos. Si uno de ellos muere por un error inesperado, el otro
    proceso también muere. Esto es útil para detener errores de forma rápida y reiniciar grupos completos de procesos.
  </p>

  <h3>Funciones principales</h3>
  <ul>
    <li><text-code>link(Pid)</text-code>: crea un link con otro proceso.</li>
    <li><text-code>unlink(Pid)</text-code>: elimina un link.</li>
    <li><text-code>spawn_link(Function)</text-code>: crea un proceso y lo linkea en una operación atómica.</li>
  </ul>

  <h3>Ejemplo de proceso simple</h3>"""

  subunit3_3_code1 = """-module(linkmon).
-compile(export_all).
    
myproc() ->
  timer:sleep(500),
  exit(reason).

start() ->
  io:format("~p~n",[spawn(fun linkmon:myproc/0)]),
  io:format("~p~n",[link(spawn(fun linkmon:myproc/0))])."""

  subunit3_3_info2 = """  <p>Si lo ejecutas con <text-code>spawn</text-code> y luego <text-code>link</text-code>, el shell puede morir cuando el
    proceso muere. Este es otro ejemplo en la Shell</p>
  <pre><code>1&gt; c(linkmon). 
  {ok,linkmon}
2&gt; spawn(fun linkmon:myproc/0).
&lt;0.52.0&gt;
3&gt; link(spawn(fun linkmon:myproc/0)).
true
** exception error: reason</code></pre>

  <h2>Propagación de Errores en Cadenas de Procesos</h2>
  <p>Podemos enlazar múltiples procesos para que todos mueran cuando alguno falle:</p>"""

  subunit3_3_code2 = """-module(linkmon).
-compile(export_all).
  
chain(0) ->
    receive _ -> ok
    after 2000 -> exit("chain dies here") end;
chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive _ -> ok end.
start() ->
    io:format("~p~n",[link(spawn(linkmon, chain, [3]))])."""
  
  subunit3_3_info3 = """  <p>Esto permite propagar errores de forma controlada y reiniciar procesos cuando sea necesario.</p>
  <p>Aquí, por ejemplo, habrá muchos procesos vinculados entre sí, muriendo a medida que cada uno de sus sucesores
    salga. Ejemplo en la Shell.</p>
  <pre><code>4&gt; c(linkmon).              
{ok,linkmon}
5&gt; link(spawn(linkmon, chain, [3])).
true
** exception error: "chain dies here"</code></pre>

  <h2>Procesos con <em>trap_exit</em></h2>
  <p>Si queremos reiniciar procesos cuando mueren sin morir nosotros mismos, usamos <text-code>process_flag(trap_exit,
      true)</text-code>.</p>
  <pre><code>1&gt; process_flag(trap_exit, true). 
true
2&gt; spawn_link(fun() -> linkmon:chain(3) end).
&lt;0.49.0&gt;
3&gt; receive X -> X end.
{'EXIT',&lt;0.49.0&gt;,"chain dies here"}</code></pre>

  <p>Esto convierte señales de salida en mensajes regulares, permitiendo un manejo seguro y reinicios.</p>

  <h2>Excepciones y Señales</h2>
  <p>Las señales de exit son mensajes especiales que no pueden atraparse con <text-code>try ... catch</text-code>.
    Algunos ejemplos:</p>
  <ul>
    <li><text-code>exit(reason)</text-code>: termina el proceso con una razón.</li>
    <li><text-code>exit(Pid, reason)</text-code>: termina otro proceso.</li>
    <li><text-code>exit(kill)</text-code>: termina de manera brutal e inatrapable.</li>
  </ul>
  <p>Monitorear procesos permite reaccionar sin afectar nuestro proceso principal.</p>

  <h2>Monitores</h2>
  <p>Los <strong>monitores</strong> son un tipo especial de link que son:</p>
  <ul>
    <li>Unidireccionales</li>
    <li>Apilables (stackable)</li>
  </ul>
  <p>Esto los hace ideales para bibliotecas que necesitan saber si un proceso está activo sin afectar a otros.</p>

  <h3>Ejemplo de monitor</h3>"""

  subunit3_3_code3 = """-module(monitoring).
-export([start/0]).

start() ->
    {Pid, Ref} = spawn_monitor(fun() -> timer:sleep(500) end),
    Pid ! die,
    receive
        {'DOWN',Ref,process,Pid,_Msg} = DownMsg ->
            io:format("~p~n", [DownMsg])
    after 1000 ->
        ok
    end."""
  
  subunit3_3_info4 = """  <p>Cuando el proceso monitoreado muere, se recibe un mensaje <text-code>{'DOWN', Ref, process, Pid,
      Reason}</text-code>.</p>

  <table>
    <thead>
      <tr>
        <th>Elemento</th>
        <th>Tipo / Valor</th>
        <th>Descripción</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td><text-code>'DOWN'</text-code></td>
        <td>Átomo</td>
        <td>Indica que este mensaje es una notificación de que un proceso monitoreado ha terminado.</td>
      </tr>
      <tr>
        <td><text-code>MonitorReference</text-code></td>
        <td>Referencia única</td>
        <td>Referencia creada por <text-code>erlang:monitor/2</text-code> que permite identificar y demonitorizar el
          proceso.</td>
      </tr>
      <tr>
        <td><text-code>process</text-code></td>
        <td>Átomo</td>
        <td>Indica que el objeto monitoreado es un proceso.</td>
      </tr>
      <tr>
        <td><text-code>Pid</text-code></td>
        <td>Identificador de proceso</td>
        <td>El PID del proceso que terminó. Útil para saber cuál de varios procesos monitoreados finalizó.</td>
      </tr>
      <tr>
        <td><text-code>Reason</text-code></td>
        <td>Átomo o cadena</td>
        <td>Razón por la cual el proceso terminó. Puede ser <text-code>normal</text-code>, <text-code>kill</text-code>,
          una excepción o cualquier otra razón de salida.</td>
      </tr>
    </tbody>
  </table>
  
  <p>También, al igual que con los enlaces, existe una función atómica para generar un proceso mientras lo monitorea, spawn_monitor/1-3:</p>

  <pre><code>1&gt; f().
ok
2&gt; {Pid, Ref} = spawn_monitor(fun() -> receive _ -> exit(boom) end end).
{&lt;0.35.0&gt;,#Ref&lt;0.0.0.35&gt;}
3&gt; Pid ! die.
die
4&gt; erlang:demonitor(Ref, [flush, info]).
false
5&gt; flush().
ok</code></pre>

<p>En lo que respecta a la función <text-code>erlang:demonitor/2</text-code>, que se encarga de dejar de monitorizar un proceso, su segundo parámetro puede ser una lista de opciones. Solo existen dos: <em>info</em> y <em>flush</em>. La opción <em>info</em> indica si existía un monitor al intentar eliminarlo. Por eso, la expresión 4 devolvió <text-code>false</text-code>.</p>

<h2>Naming y Comunicación entre Procesos</h2>

<p>Todavia tenemos un problema en cuestión al manejo de los procesos, vamos a verlo con el siguiente ejemplo, Tenemos el modulo <text-code>linkmon</text-code> con el cual recibiremos una crítica dependiendo del valor que enviemos:</p>"""

  subunit3_3_code4 = """-module(linkmon).
-compile(export_all).
  
start_critic() ->
  spawn(linkmon, critic, []).
 
judge(Pid, Band, Album) ->
  Pid ! {self(), {Band, Album}},
  receive
    {Pid, Criticism} -> Criticism
  after 2000 ->
    timeout
  end.
 
critic() ->
  receive
    {From, {"Rage Against the Turing Machine", "Unit Testify"}} ->
      From ! {self(), "They are great!"};
    {From, {"System of a Downtime", "Memoize"}} ->
      From ! {self(), "They're not Johnny Crash but they're good."};
    {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
      From ! {self(), "Simply incredible."};
    {From, {_Band, _Album}} ->
      From ! {self(), "They are terrible!"}
  end,
  critic().

start() ->
  Critic = start_critic(),
  io:format("~p~n", [judge(Critic, "Genesis", "The Lambda Lies Down on Broadway")])."""

  subunit3_3_info5 = """<p>Pero si el proceso muere, no volveremos a recibir una crítica</p>

<pre><code>4&gt; exit(Critic, solar_storm).
true
5&gt; linkmon:judge(Critic, "Genesis", "A trick of the Tail Recursion").
timeout
</code></pre>

<p>Por lo tanto, para mantener vivo el proceso crítico, podemos usar un proceso supervisor para reiniciarlo automáticamente.</p>

<h3>Supervisor y procesos nombrados</h3>
<p>Si el crítico muere, podemos usar un proceso supervisor para reiniciarlo automáticamente:</p>
<pre><code>start_critic2() -&gt; spawn(?MODULE, restarter, []).

restarter() -&gt;
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic2, []),
    receive
        {'EXIT', Pid, normal} -&gt; ok;
        {'EXIT', Pid, shutdown} -&gt; ok;
        {'EXIT', Pid, _} -&gt; restarter()
    end.</code></pre>

<p>Ahora podemos reiniciar el proceso, Bien, pero en cambio nos encontramos con otro problema de esta solución la cual es que necesitaremos del Pid del proceso del crítico, si no, no podremos reiniciarlo. Por lo tanto una solución a esto es nombrar el proceso</p>

<pre><code>start_critic2() -&gt; spawn(?MODULE, restarter, []).

restarter() -&gt;
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic2, []),
    register(critic, Pid),
    receive
        {'EXIT', Pid, normal} -&gt; ok;
        {'EXIT', Pid, shutdown} -&gt; ok;
        {'EXIT', Pid, _} -&gt; restarter()
    end.</code></pre>

<h3>Problemas con procesos nombrados y race conditions</h3>
<p>Al usar <text-code>register/2</text-code>, nuestro crítico siempre tendrá el nombre <text-code>critic</text-code>, sin importar cuál sea su <text-code>Pid</text-code>. Esto nos permite eliminar la necesidad de pasar un <text-code>Pid</text-code> en nuestras funciones de abstracción, como en el siguiente ejemplo:</p>

<pre><code>judge2(Band, Album) -&gt; 
    critic ! {self(), {Band, Album}},
    Pid = whereis(critic),
    receive
      {Pid, Criticism} -&gt; Criticism
    after 2000 -&gt; 
      timeout
    end.</code></pre>

<p>La línea <text-code>Pid = whereis(critic)</text-code> obtiene el identificador de proceso del crítico para hacer un pattern match correcto en el <text-code>receive</text-code>. Esto asegura que respondamos al mensaje adecuado, incluso si hay muchos otros mensajes en la cola.</p>

<p>Sin embargo, este código tiene un problema potencial debido a la concurrencia:</p>
<ol>
<li>Se envía el mensaje: <text-code>critic ! Message</text-code></li>
<li>El crítico recibe el mensaje y responde</li>
<li>El crítico muere</li>
<li><text-code>whereis(critic)</text-code> falla</li>
<li>El crítico es reiniciado automáticamente</li>
<li>El código falla porque <text-code>Pid</text-code> ya no corresponde</li>
</ol>

<p>Otra situación posible es que <text-code>whereis(critic)</text-code> recoja un <text-code>Pid</text-code> incorrecto después del reinicio, y entonces el mensaje nunca haga match. Este tipo de problemas surgen cuando el estado de un recurso es compartido entre procesos —en este caso, el nombre <text-code>critic</text-code>— y varios procesos pueden leerlo o modificarlo simultáneamente. A esto se le llama <strong>race condition</strong>.</p>

<h3>Solución con referencias únicas</h3>
<p>Para evitar este tipo de race conditions, podemos usar <text-code>make_ref()</text-code> para generar referencias únicas y asociarlas a cada mensaje:</p>"""

  subunit3_3_code5 = """-module(linkmon).
-compile(export_all).

start_critic2() -> spawn(linkmon, restarter, []).

restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(linkmon, critic2, []),
  register(critic, Pid),
  receive
      {'EXIT', Pid, normal} -> ok;
      {'EXIT', Pid, shutdown} -> ok;
      {'EXIT', Pid, _} -> restarter()
  end.

judge2(Band, Album) ->
  Ref = make_ref(),
  critic ! {self(), Ref, {Band, Album}},
  receive
      {Ref, Criticism} -> Criticism
  after 2000 ->
      timeout
  end.

critic2() ->
  receive
      {From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
          From ! {Ref, "They are great!"};
      {From, Ref, {"System of a Downtime", "Memoize"}} ->
          From ! {Ref, "They're not Johnny Crash but they're good."};
      {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
          From ! {Ref, "Simply incredible."};
      {From, Ref, {_Band, _Album}} ->
          From ! {Ref, "They are terrible!"}
  end,
  critic2().

start() ->
  start_critic2(),
  timer:sleep(100),
  io:format("~p~n", [judge2("The Doors", "Light my Firewall")]),
  io:format("~p~n", [exit(whereis(critic), kill)]),
  timer:sleep(100),
  io:format("~p~n",[judge2("Rage Against the Turing Machine", "Unit Testify")])."""

  subunit3_3_info6 = """<p>Con este enfoque, cada mensaje tiene un identificador único (<text-code>Ref</text-code>), evitando conflictos incluso si el crítico muere y es reiniciado entre el envío y la recepción del mensaje.</p>

<p>Aunque matamos al crítico, un nuevo proceso fue reiniciado automáticamente y nuestros mensajes siguieron funcionando correctamente. Esto demuestra la utilidad de los procesos nombrados combinados con referencias únicas.</p>

<h3>Notas importantes</h3>
<ul>
<li>Los átomos en Erlang son limitados. Evita crear átomos dinámicos de manera masiva.</li>
<li>Los procesos nombrados deberían reservarse para servicios importantes que se mantengan durante toda la ejecución de la VM.</li>
<li>Para procesos transitorios, puede ser más seguro representarlos como un grupo y vincularlos con <text-code>links</text-code> y reinicios automáticos, en lugar de usar nombres dinámicos.</li>
</ul>

<p>En el próximo capítulo, aplicaremos estos conceptos escribiendo una aplicación concurrente real en Erlang.</p>"""

  subunit3_3 = SubunitCreate(
      title="Monitoreo y manejo de fallos",
      description="This is the third subunit of unit 3.",
      order=3,
      blocks=[
          {"type": "text", "value": subunit3_3_info1},
          {"type": "code", "value": subunit3_3_code1},
          {"type": "text", "value": subunit3_3_info2},
          {"type": "code", "value": subunit3_3_code2},
          {"type": "text", "value": subunit3_3_info3},
          {"type": "code", "value": subunit3_3_code3},
          {"type": "text", "value": subunit3_3_info4},
          {"type": "code", "value": subunit3_3_code4},
          {"type": "text", "value": subunit3_3_info5},
          {"type": "code", "value": subunit3_3_code5},
          {"type": "text", "value": subunit3_3_info6},
      ],
      unit_id=unit3_id
  )

  subunit1_1_db = await subunit_crud.create_subunit(db, subunit1_1)
  subunit1_2_db = await subunit_crud.create_subunit(db, subunit1_2)
  subunit1_3_db = await subunit_crud.create_subunit(db, subunit1_3)
  subunit1_4_db = await subunit_crud.create_subunit(db, subunit1_4)
  subunit1_5_db = await subunit_crud.create_subunit(db, subunit1_5)
  subunit1_6_db = await subunit_crud.create_subunit(db, subunit1_6)

  subunit2_1_db = await subunit_crud.create_subunit(db, subunit2_1)
  subunit2_2_db = await subunit_crud.create_subunit(db, subunit2_2)
  subunit2_3_db = await subunit_crud.create_subunit(db, subunit2_3)
  subunit2_4_db = await subunit_crud.create_subunit(db, subunit2_4)

  subunit3_1_db = await subunit_crud.create_subunit(db, subunit3_1)
  subunit3_2_db = await subunit_crud.create_subunit(db, subunit3_2)
  subunit3_3_db = await subunit_crud.create_subunit(db, subunit3_3)

  descripcion_primer_ejercicio = """
<p>
  Crea un módulo llamado <text-code>ejercicio</text-code> con una función pública <text-code>start/1</text-code> que reciba una lista de tuplas con el formato:
</p>

<pre><code>{Producto, Precio, Categoria}</code></pre>

<p>donde:</p>
<ul>
  <li><text-code>Producto</text-code> es un átomo (ej: <text-code>manzana</text-code>)</li>
  <li><text-code>Precio</text-code> es un entero</li>
  <li><text-code>Categoria</text-code> es un átomo (<text-code>fruta</text-code>, <text-code>bebida</text-code>, <text-code>otro</text-code>)</li>
</ul>

<p>La función debe:</p>
<ol>
  <li>Filtrar los productos cuyo precio sea mayor que 10 usando <strong>comprensión de listas</strong>.</li>
  <li>Clasificar el resultado en una tupla <text-code>{caros, baratos}</text-code> usando <strong>pattern matching</strong> y <strong>guardas</strong> (<text-code>precio &gt; 50</text-code> → <text-code>caros</text-code>, lo demás → <text-code>baratos</text-code>).</li>
  <li>Usar un <strong>case</strong> para devolver:
    <ul>
      <li><text-code>{"Muchos productos caros", Lista}</text-code> si <text-code>caros</text-code> tiene 3 o más elementos.</li>
      <li><text-code>{"Pocos productos caros", Lista}</text-code> en cualquier otro caso.</li>
    </ul>
  </li>
</ol>"""

  esquema_1 = """-module(ejercicio).
-export([start/1]).

%% start/1 recibe una lista de tuplas {Producto, Precio, Categoria}
start(Productos) ->
    %% 1. Filtrar los productos cuyo precio sea > 10
    Filtrados = [], %% TODO: usar list comprehension

    %% 2. Separar en caros y baratos
    Caros = [],    %% TODO: filtrar precio > 50
    Baratos = [],  %% TODO: filtrar el resto

    %% 3. Devolver un mensaje según cantidad de caros
    case 0 of   %% TODO: reemplazar con length(Caros)
        _ -> {"Pocos productos caros", {Caros, Baratos}}
    end.

"""
  
  pruebas_1 = """defmodule EjercicioTest do
use ExUnit.Case

test "clasifica correctamente productos y detecta muchos caros" do
  productos = [
    {:manzana, 60, :fruta},
    {:pera, 70, :fruta},
    {:vino, 100, :bebida},
    {:agua, 5, :bebida},
    {:pan, 20, :otro}
  ]

  assert :ejercicio.start(productos) ==
          {'Muchos productos caros',
          {[{:manzana, 60, :fruta},
            {:pera, 70, :fruta},
            {:vino, 100, :bebida}],
            [{:pan, 20, :otro}]}}
end

test "clasifica correctamente productos y detecta pocos caros" do
  productos = [
    {:manzana, 60, :fruta},
    {:agua, 5, :bebida},
    {:pan, 20, :otro}
  ]

  assert :ejercicio.start(productos) ==
          {'Pocos productos caros',
          {[{:manzana, 60, :fruta}],
            [{:pan, 20, :otro}]}}
end
end"""

  exercise1 = ExerciseCreate(
      title="Ejercicio 1",
      description=descripcion_primer_ejercicio,
      exercise_schema=esquema_1,
      test_cases=pruebas_1,
      unit_id=unit1_id
  )

  await exercise_crud.create_exercise(db, exercise1)


  descripcion_segundo_ejercicio = """   <p>
      Se desea implementar un <strong>registro de estudiantes</strong> con sus notas en distintas materias y realizar varias operaciones utilizando Erlang.
    </p>

    <h2>Objetivos</h2>
    <ul>
      <li>Practicar <strong>funciones anónimas</strong> y de <strong>orden superior</strong>.</li>
      <li>Aplicar <strong>recursión</strong> para filtrar y contar datos.</li>
      <li>Trabajar con <strong>estructuras de datos</strong>: records, mapas y conjuntos.</li>
      <li>Manejo de <strong>errores y excepciones</strong> al acceder a datos.</li>
    </ul>

    <h2>Requerimientos</h2>
    <ol>
      <li>Crear un <code>record</code> <strong>student</strong> con campos: <text-code>id</text-code>, <text-code>name</text-code> y <text-code>grades</text-code> (lista de <text-code>{materia, nota}</text-code>).</li>
      <li>Implementar una <strong>función anónima</strong> que reciba un estudiante y devuelva su promedio.</li>
      <li>Crear una <strong>función de orden superior</strong> que reciba la lista de estudiantes y una función, y devuelva la lista de resultados aplicando la función a cada estudiante.</li>
      <li>Implementar <strong>recursión</strong> para:
        <ul>
          <li>Filtrar estudiantes que tengan promedio mayor o igual a 70.</li>
          <li>Contar cuántos estudiantes cumplen la condición.</li>
        </ul>
      </li>
      <li>Guardar los estudiantes en una <strong>estructura clave/valor</strong> (mapas) y poder buscar por <text-code>id</text-code>.</li>
      <li>Manejar posibles <strong>errores</strong> al buscar un <text-code>id</text-code> que no exista.</li>
    </ol>

    <h2>Notas</h2>
    <p>
      Se espera que combines todos los conceptos vistos en la unidad, aplicando recursión, funciones de orden superior, estructuras de datos y manejo de excepciones en Erlang.
    </p>"""
  
  esquema_2 = """-module(student_manager).
-export([average/1, apply_to_all/2, filter_passing/1, count_passing/1, add_student/2, get_student/2]).

-record(student, {id, name, grades}).
%% Record de ejemplo: #student {id = 1, name= "Alicia", grades = [{"Math", 80}, {"Sci", 90}]}


% 1. Función anónima para promedio
average(Student) ->
    %% TODO: implementar cálculo del promedio usando función anónima
    ok.

% 2. Función de orden superior
apply_to_all(Students, Fun) ->
    %% TODO: aplicar Fun a cada estudiante
    ok.

% 3. Filtrar estudiantes aprobados (recursión)
filter_passing(Students) ->
    %% TODO: devolver lista de estudiantes con promedio >= 70
    ok.

% 4. Contar estudiantes aprobados (recursión)
count_passing(Students) ->
    %% TODO: devolver número de estudiantes con promedio >= 70
    ok.

% 5. Añadir estudiante a estructura clave/valor
add_student(Map, Student) ->
    %% TODO: agregar Student al map usando id como clave
    ok.

% 6. Buscar estudiante por id con manejo de error
get_student(Map, Id) ->
    %% TODO: devolver estudiante o {:error, not_found}
    ok.
"""

  pruebas_2 = """defmodule StudentManagerTest do
  use ExUnit.Case

  test "calcula promedio correctamente" do
    # Aquí se llamaría a la función de Erlang via :student_manager.average/1
    assert :student_manager.average({:student, 1, "Ana", [{"Math", 80}, {"Sci", 90}]}) == 85
  end

  test "aplica función de orden superior" do
    students = [
      {:student, 1, "Ana", [{"Math", 80}, {"Sci", 90}]},
      {:student, 2, "Luis", [{"Math", 60}, {"Sci", 70}]}
    ]
    # Aplicar promedio
    result = :student_manager.apply_to_all(students, &:student_manager.average/1)
    assert result == [85, 65]
  end

  test "filtra estudiantes aprobados" do
    students = [
      {:student, 1, "Ana", [{"Math", 80}, {"Sci", 90}]},
      {:student, 2, "Luis", [{"Math", 60}, {"Sci", 70}]}
    ]
    passing = :student_manager.filter_passing(students)
    assert passing == [{:student, 1, "Ana", [{"Math", 80}, {"Sci", 90}]}]
  end

  test "maneja búsqueda de estudiante con error" do
    map = :student_manager.add_student(%{}, {:student, 1, "Ana", [{"Math", 80}]})
    assert :student_manager.get_student(map, 2) == {:error, :not_found}
  end
end
"""

  exercise2 = ExerciseCreate(
      title="Ejercicio: Registro de Estudiantes",
      description=descripcion_segundo_ejercicio,
      exercise_schema=esquema_2,
      test_cases=pruebas_2,
      unit_id=unit2_id
  )

  await exercise_crud.create_exercise(db, exercise2)

  descripcion_tercer_ejercicio = """   
  <h2>Objetivo</h2>
    <p>Practicar la creación y manejo de procesos concurrentes en Erlang, la comunicación entre ellos y la monitorización de fallos. Aprenderás a coordinar procesos que intercambian mensajes y a manejar situaciones de error de manera controlada.</p>

    <h2>Descripción</h2>
    <p>Se debe implementar un sistema simple de "Ping-Pong" usando procesos en Erlang:</p>

    <ul>
        <li>
            <strong>Proceso árbitro (Referee):</strong>
            <ul>
                <li>Inicia el sistema.</li>
                <li>Crea los procesos <text-code>ping</text-code> y <text-code>pong</text-code>.</li>
                <li>Monitoriza ambos procesos para detectar fallos.</li>
                <li>Mantiene un loop activo para recibir notificaciones de procesos caídos (<text-code>'DOWN'</text-code> messages).</li>
            </ul>
        </li>
        <li>
            <strong>Proceso Ping:</strong>
            <ul>
                <li>Envía mensajes "ping" al proceso <text-code>pong</text-code>.</li>
                <li>Espera la respuesta "pong".</li>
                <li>Repite este intercambio un número determinado de veces (<text-code>N</text-code>).</li>
                <li>Termina de forma normal al completar los intercambios.</li>
            </ul>
        </li>
        <li>
            <strong>Proceso Pong:</strong>
            <ul>
                <li>Espera mensajes "ping" del proceso <text-code>ping</text-code>.</li>
                <li>Responde con "pong".</li>
                <li>Mantiene un loop continuo hasta ser terminado.</li>
            </ul>
        </li>
    </ul>

    <h2>Requisitos de aprendizaje</h2>
    <ul>
        <li>Crear procesos usando <text-code>spawn</text-code>.</li>
        <li>Establecer comunicación entre procesos mediante <text-code>send</text-code> y <text-code>receive</text-code>.</li>
        <li>Monitorear procesos con <text-code>monitor</text-code> y manejar mensajes <text-code>'DOWN'</text-code>.</li>
        <li>Gestionar la terminación correcta de procesos.</li>
        <li>Mantener un proceso supervisor (referee) activo para supervisar otros procesos.</li>
    </ul>

    <h2>Extras / Retos opcionales</h2>
    <ul>
        <li>Manejar el reinicio de procesos que fallen.</li>
        <li>Probar el sistema con diferentes cantidades de intercambios.</li>
        <li>Añadir mecanismos para reportar el estado de los procesos al proceso de prueba (test process).</li>
    </ul>

    <h2>Resultado esperado</h2>
    <ul>
        <li>El árbitro permanece activo mientras monitorea los procesos <text-code>ping</text-code> y <text-code>pong</text-code>.</li>
        <li><text-code>Ping</text-code> termina después de completar los intercambios especificados.</li>
        <li><text-code>Pong</text-code> puede seguir en loop o terminar al recibir señal de cierre.</li>
        <li class="important">Cualquier fallo de <text-code>ping</text-code> o <text-code>pong</text-code> es detectado y manejado por el árbitro sin que el sistema completo colapse.</li>
    </ul>"""

  esquema_3 = """-module(pingpong).
-export([start/0, referee/0, ping/2, pong/0]).

%% Entry point
%% Responsibilities:
%% - Spawn the referee process
start() ->
    %% TODO: spawn referee process
    ok.

%% Referee process
%% Responsibilities:
%% - Spawn ping and pong processes
%% - Monitor them (link/monitor)
%% - Handle DOWN messages if a process crashes
%% - Keep loop alive for monitoring
referee() ->
    receive
        start ->
            %% TODO: spawn pong process and monitor it
            %% TODO: spawn ping process and monitor it
            %% TODO: send initial message to ping
            loop()
    end.

%% Referee loop
%% Responsibilities:
%% - Handle DOWN messages from ping or pong
%% - Print/log crash info
%% - Keep looping
loop() ->
    receive
        {'DOWN', _Ref, process, Pid, Reason} ->
            %% TODO: log or handle crash
            loop()
    end.

%% Ping process
%% Parameters: PongPid, RemainingCount
%% Responsibilities:
%% - Send ping message to pong
%% - Wait for pong reply
%% - Loop until RemainingCount reaches 0
%% - Exit normally after finishing
ping(PongPid, Count) ->
    %% TODO: implement ping logic
    ok.

%% Pong process
%% Responsibilities:
%% - Wait for ping message
%% - Reply with pong
%% - Loop forever (or until terminated)
pong() ->
    %% TODO: implement pong logic
    ok.
"""

  pruebas_3 = """defmodule PingPongTest do
  use ExUnit.Case

  test "ping-pong exchanges messages N times and terminates" do
    # Start the referee process (spawns ping and pong)
    referee_pid = :erlang.spawn(:pingpong, :referee, [])
    
    # Kick off the ping-pong
    send(referee_pid, :start)

    # Give some time for the ping-pong to finish
    Process.sleep(1000)

    # Assert that referee, ping, and pong processes are no longer alive
    # (they should terminate normally after N exchanges)
    ref_state = Process.alive?(referee_pid)
    
    assert ref_state == true   # referee loop is still alive, monitoring DOWN messages

    # Note: ping and pong exit normally, but we need their PIDs
    # If you want to track them, the referee process could send their PIDs back
    # For simplicity, we can test that a normal run does not crash
    assert true
  end

  test "referee monitors crashing process" do
    # This test requires modifying pong to crash after a few messages
    referee_pid = :erlang.spawn(:pingpong, :referee, [])
    send(referee_pid, :start)

    # Sleep a bit for the crash to happen
    Process.sleep(1000)

    # Ideally, the referee loop should be alive and print DOWN message
    assert Process.alive?(referee_pid)
  end

  test "ping sends exactly N messages" do
  test_pid = self()
  referee_pid = :erlang.spawn(:pingpong, :referee, [])
  send(referee_pid, {:start, test_pid})

  # Optionally, have referee send ping/pong PIDs to test process
  # Collect messages sent by ping
  Process.sleep(1000)

  # assert_received ping messages == N
  # Example (if you modify ping to send {:ping, count} to test_pid)
  end

  test "referee handles multiple crashing processes" do
  referee_pid = :erlang.spawn(:pingpong, :referee, [])
  send(referee_pid, :start)

  # Send messages to simulate crashes
  # send(referee_pid, {:crash_ping})
  # send(referee_pid, {:crash_pong})

  Process.sleep(1000)

  assert Process.alive?(referee_pid)
  # assert_received {:down, _pid, _reason}  # twice
  end
end
"""

  exercise3 = ExerciseCreate(
      title="Ejercicio: Sistema Ping-Pong con Concurrencia en Erlang",
      description=descripcion_tercer_ejercicio,
      exercise_schema=esquema_3,
      test_cases=pruebas_3,
      unit_id=unit3_id
  )

  await exercise_crud.create_exercise(db, exercise3)

  # Crear el quiz
  quiz1 = QuizCreate(
      title="Quiz Erlang: Introducción y Sintaxis",
      description="Quiz sobre los conceptos fundamentales de Erlang, su sintaxis y características.",
      subunit_id=subunit1_1_db.id
  )
  quiz1_db = await quiz_crud.create_quiz(db, quiz1)
  quiz1_id = quiz1_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="¿Qué es Erlang?",
      quiz_id=quiz1_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  option1 = OptionCreate(
      text="Un lenguaje de programación de propósito general, concurrente y funcional.",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  )
  option2 = OptionCreate(
      text="Una base de datos relacional.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option3 = OptionCreate(
      text="Un sistema operativo.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option4 = OptionCreate(
      text="Un lenguaje orientado a objetos.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="¿Cuál es la filosofía de manejo de errores de Erlang?",
      quiz_id=quiz1_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  option1 = OptionCreate(
      text="Ignorar los errores.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option2 = OptionCreate(
      text="Let it crash!",
      is_correct=True,
      quiz_question_id=quiz_question2_id
  )
  option3 = OptionCreate(
      text="Reiniciar el sistema cada hora.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option4 = OptionCreate(
      text="Manejar errores con try-catch extensos.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="¿En qué año y dónde se desarrolló Erlang?",
      quiz_id=quiz1_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  option1 = OptionCreate(
      text="1986 en los Laboratorios de Ericsson.",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  )
  option2 = OptionCreate(
      text="1995 en Microsoft Labs.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option3 = OptionCreate(
      text="2000 en IBM Research.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option4 = OptionCreate(
      text="1978 en Bell Labs.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="En Erlang, las variables son:",
      quiz_id=quiz1_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  option1 = OptionCreate(
      text="Mutables y pueden reasignarse.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option2 = OptionCreate(
      text="Inmutables una vez asignadas.",
      is_correct=True,
      quiz_question_id=quiz_question4_id
  )
  option3 = OptionCreate(
      text="Siempre se escriben en minúscula.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option4 = OptionCreate(
      text="No existen en Erlang.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 5
  quiz_question5 = QuizQuestionCreate(
      question_text="¿Qué hace io:format en Erlang?",
      quiz_id=quiz1_id
  )
  quiz_question5_db = await quiz_crud.create_quiz_question(db, quiz_question5)
  quiz_question5_id = quiz_question5_db.id

  option1 = OptionCreate(
      text="Imprime texto en la consola.",
      is_correct=True,
      quiz_question_id=quiz_question5_id
  )
  option2 = OptionCreate(
      text="Define un módulo.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option3 = OptionCreate(
      text="Crea un proceso concurrente.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option4 = OptionCreate(
      text="Declara una variable.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)


  # Crear el quiz 2
  quiz2 = QuizCreate(
      title="Quiz Erlang: Instalación",
      description="Quiz sobre cómo instalar Erlang en Linux, macOS y Windows.",
      subunit_id=subunit1_2_db.id
  )
  quiz2_db = await quiz_crud.create_quiz(db, quiz2)
  quiz2_id = quiz2_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="En distribuciones basadas en Debian/Ubuntu, ¿cómo se instala Erlang?",
      quiz_id=quiz2_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  option1 = OptionCreate(
      text="sudo apt install erlang",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  )
  option2 = OptionCreate(
      text="sudo dnf install erlang",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option3 = OptionCreate(
      text="brew install erlang",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option4 = OptionCreate(
      text="choco install erlang",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="¿Cómo se verifica la instalación de Erlang en cualquier sistema operativo?",
      quiz_id=quiz2_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  option1 = OptionCreate(
      text="Ejecutando 'erl' en la terminal o CMD/PowerShell.",
      is_correct=True,
      quiz_question_id=quiz_question2_id
  )
  option2 = OptionCreate(
      text="Ejecutando 'erlang -v'.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option3 = OptionCreate(
      text="Abrir cualquier navegador web.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option4 = OptionCreate(
      text="Usando el comando 'run_erlang'.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="En macOS, ¿qué herramienta se utiliza comúnmente para instalar Erlang?",
      quiz_id=quiz2_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  option1 = OptionCreate(
      text="Homebrew",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  )
  option2 = OptionCreate(
      text="APT",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option3 = OptionCreate(
      text="Chocolatey",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option4 = OptionCreate(
      text="Yum",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="En Windows, ¿cuál es una opción para instalar Erlang mediante un gestor de paquetes?",
      quiz_id=quiz2_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  option1 = OptionCreate(
      text="APT",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option2 = OptionCreate(
      text="Homebrew",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option3 = OptionCreate(
      text="Chocolatey",
      is_correct=True,
      quiz_question_id=quiz_question4_id
  )
  option4 = OptionCreate(
      text="Yum",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 5
  quiz_question5 = QuizQuestionCreate(
      question_text="Para proyectos en Elixir o RabbitMQ, ¿qué se recomienda a veces al instalar Erlang?",
      quiz_id=quiz2_id
  )
  quiz_question5_db = await quiz_crud.create_quiz_question(db, quiz_question5)
  quiz_question5_id = quiz_question5_db.id

  option1 = OptionCreate(
      text="Instalar siempre la versión más reciente sin considerar compatibilidad.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option2 = OptionCreate(
      text="Usar versiones específicas desde repositorios oficiales o gestores de versiones como asdf.",
      is_correct=True,
      quiz_question_id=quiz_question5_id
  )
  option3 = OptionCreate(
      text="Instalar Erlang solo en Windows.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option4 = OptionCreate(
      text="No es necesario instalar Erlang para Elixir o RabbitMQ.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Crear el quiz 3
  quiz3 = QuizCreate(
      title="Quiz Erlang: Shell y Operaciones",
      description="Quiz sobre el uso de la Shell de Erlang, comandos y operaciones básicas.",
      subunit_id=subunit1_3_db.id
  )
  quiz3_db = await quiz_crud.create_quiz(db, quiz3)
  quiz3_id = quiz3_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="¿Qué comando abre la Shell de Erlang en Linux y macOS?",
      quiz_id=quiz3_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  option1 = OptionCreate(
      text="erl",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  )
  option2 = OptionCreate(
      text="werl",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option3 = OptionCreate(
      text="erlang",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option4 = OptionCreate(
      text="eshell",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="En Windows, ¿qué shell de Erlang tiene su propia ventana con barras de desplazamiento y permite copiar/pegar fácilmente?",
      quiz_id=quiz3_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  option1 = OptionCreate(
      text="erl.exe",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option2 = OptionCreate(
      text="werl.exe",
      is_correct=True,
      quiz_question_id=quiz_question2_id
  )
  option3 = OptionCreate(
      text="cmd.exe",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option4 = OptionCreate(
      text="powershell.exe",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="¿Qué comando en la Shell de Erlang se usa para salir?",
      quiz_id=quiz3_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  option1 = OptionCreate(
      text="q().",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  )
  option2 = OptionCreate(
      text="exit().",
      is_correct=True,  # Ambas son correctas
      quiz_question_id=quiz_question3_id
  )
  option3 = OptionCreate(
      text="f().",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option4 = OptionCreate(
      text="c().",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="¿Cuál de las siguientes operaciones aritméticas en Erlang devuelve un número entero?",
      quiz_id=quiz3_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  option1 = OptionCreate(
      text="10 / 4.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option2 = OptionCreate(
      text="10 div 4.",
      is_correct=True,
      quiz_question_id=quiz_question4_id
  )
  option3 = OptionCreate(
      text="10 + 5.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option4 = OptionCreate(
      text="10 * 2.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 5
  quiz_question5 = QuizQuestionCreate(
      question_text="¿Cuál es el resultado de la operación 'true andalso (2 > 1).' en la Shell de Erlang?",
      quiz_id=quiz3_id
  )
  quiz_question5_db = await quiz_crud.create_quiz_question(db, quiz_question5)
  quiz_question5_id = quiz_question5_db.id

  option1 = OptionCreate(
      text="false",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option2 = OptionCreate(
      text="true",
      is_correct=True,
      quiz_question_id=quiz_question5_id
  )
  option3 = OptionCreate(
      text="error",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option4 = OptionCreate(
      text="undefined",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 6
  quiz_question6 = QuizQuestionCreate(
      question_text="En Erlang, ¿qué operador se utiliza para obtener el primer elemento de una lista?",
      quiz_id=quiz3_id
  )
  quiz_question6_db = await quiz_crud.create_quiz_question(db, quiz_question6)
  quiz_question6_id = quiz_question6_db.id

  option1 = OptionCreate(
      text="hd([1,2,3]).",
      is_correct=True,
      quiz_question_id=quiz_question6_id
  )
  option2 = OptionCreate(
      text="tl([1,2,3]).",
      is_correct=False,
      quiz_question_id=quiz_question6_id
  )
  option3 = OptionCreate(
      text="element(1, [1,2,3]).",
      is_correct=False,
      quiz_question_id=quiz_question6_id
  )
  option4 = OptionCreate(
      text="head([1,2,3]).",
      is_correct=False,
      quiz_question_id=quiz_question6_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Crear el quiz 4
  quiz4 = QuizCreate(
      title="Quiz Erlang: Tipos de Datos",
      description="Quiz sobre los tipos de datos fundamentales en Erlang: enteros, variables, átomos, booleanos, tuplas y listas.",
      subunit_id=subunit1_4_db.id
  )
  quiz4_db = await quiz_crud.create_quiz(db, quiz4)
  quiz4_id = quiz4_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="¿Cuál de los siguientes es un entero negativo en Erlang?",
      quiz_id=quiz4_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  option1 = OptionCreate(text="42", is_correct=False, quiz_question_id=quiz_question1_id)
  option2 = OptionCreate(text="-42", is_correct=True, quiz_question_id=quiz_question1_id)
  option3 = OptionCreate(text="2#101010", is_correct=False, quiz_question_id=quiz_question1_id)
  option4 = OptionCreate(text="16#AE", is_correct=False, quiz_question_id=quiz_question1_id)
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="¿Qué afirmación sobre las variables en Erlang es correcta?",
      quiz_id=quiz4_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  option1 = OptionCreate(text="Se pueden reasignar libremente.", is_correct=False, quiz_question_id=quiz_question2_id)
  option2 = OptionCreate(text="Su valor es inmutable y se asigna solo una vez.", is_correct=True, quiz_question_id=quiz_question2_id)
  option3 = OptionCreate(text="No se pueden usar en operaciones aritméticas.", is_correct=False, quiz_question_id=quiz_question2_id)
  option4 = OptionCreate(text="Empiezan siempre con letra minúscula.", is_correct=False, quiz_question_id=quiz_question2_id)
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="¿Qué es un átomo en Erlang?",
      quiz_id=quiz4_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  option1 = OptionCreate(text="Una variable que puede cambiar su valor.", is_correct=False, quiz_question_id=quiz_question3_id)
  option2 = OptionCreate(text="Un número entero positivo.", is_correct=False, quiz_question_id=quiz_question3_id)
  option3 = OptionCreate(text="Una constante cuyo valor es su propio nombre.", is_correct=True, quiz_question_id=quiz_question3_id)
  option4 = OptionCreate(text="Un tipo de lista especial.", is_correct=False, quiz_question_id=quiz_question3_id)
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="¿Cuál de las siguientes expresiones devuelve true?",
      quiz_id=quiz4_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  option1 = OptionCreate(text="true and false", is_correct=False, quiz_question_id=quiz_question4_id)
  option2 = OptionCreate(text="not false", is_correct=True, quiz_question_id=quiz_question4_id)
  option3 = OptionCreate(text="1 =:= 0", is_correct=False, quiz_question_id=quiz_question4_id)
  option4 = OptionCreate(text="5 =:= 5.0", is_correct=False, quiz_question_id=quiz_question4_id)
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 5
  quiz_question5 = QuizQuestionCreate(
      question_text="¿Cómo se define una tupla con los valores X=10 y Y=4?",
      quiz_id=quiz4_id
  )
  quiz_question5_db = await quiz_crud.create_quiz_question(db, quiz_question5)
  quiz_question5_id = quiz_question5_db.id

  option1 = OptionCreate(text="{X, Y}", is_correct=True, quiz_question_id=quiz_question5_id)
  option2 = OptionCreate(text="[X, Y]", is_correct=False, quiz_question_id=quiz_question5_id)
  option3 = OptionCreate(text="(X, Y)", is_correct=False, quiz_question_id=quiz_question5_id)
  option4 = OptionCreate(text="{X+Y}", is_correct=False, quiz_question_id=quiz_question5_id)
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 6
  quiz_question6 = QuizQuestionCreate(
      question_text="¿Cuál de las siguientes listas es propia en Erlang?",
      quiz_id=quiz4_id
  )
  quiz_question6_db = await quiz_crud.create_quiz_question(db, quiz_question6)
  quiz_question6_id = quiz_question6_db.id

  option1 = OptionCreate(text="[1,2,3]", is_correct=True, quiz_question_id=quiz_question6_id)
  option2 = OptionCreate(text="[1 | 2]", is_correct=False, quiz_question_id=quiz_question6_id)
  option3 = OptionCreate(text="[a | b]", is_correct=False, quiz_question_id=quiz_question6_id)
  option4 = OptionCreate(text="[1 | 2,3]", is_correct=False, quiz_question_id=quiz_question6_id)
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

    # Crear el quiz 5
  quiz5 = QuizCreate(
      title="Quiz Erlang: Módulos y Funciones",
      description="Quiz sobre la creación, uso y compilación de módulos en Erlang.",
      subunit_id=subunit1_5_db.id
  )
  quiz5_db = await quiz_crud.create_quiz(db, quiz5)
  quiz5_id = quiz5_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="¿Cuál es el propósito principal de un módulo en Erlang?",
      quiz_id=quiz5_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  option1 = OptionCreate(
      text="Agrupar funciones bajo un solo nombre para reutilización.",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  )
  option2 = OptionCreate(
      text="Almacenar variables globales.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option3 = OptionCreate(
      text="Ejecutar código más rápido que el shell interactivo.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option4 = OptionCreate(
      text="Permitir modificar valores de variables en ejecución.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="¿Cuál es el atributo obligatorio que debe tener un módulo?",
      quiz_id=quiz5_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  option1 = OptionCreate(
      text="-module(Nombre).",
      is_correct=True,
      quiz_question_id=quiz_question2_id
  )
  option2 = OptionCreate(
      text="-export([Funciones/Aridad]).",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option3 = OptionCreate(
      text="-import(Modulo, [Funciones/Aridad]).",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option4 = OptionCreate(
      text="-compile([Flags]).",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="Si tienes un módulo llamado useless con una función add/2, ¿cómo la llamas desde el shell?",
      quiz_id=quiz5_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  option1 = OptionCreate(
      text="useless:add(7,2).",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  )
  option2 = OptionCreate(
      text="add(7,2).",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option3 = OptionCreate(
      text="useless.add(7,2).",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option4 = OptionCreate(
      text="call(useless, add, 7, 2).",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="¿Qué comando compila un módulo llamado useless en el shell de Erlang?",
      quiz_id=quiz5_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  option1 = OptionCreate(
      text="c(useless).",
      is_correct=True,
      quiz_question_id=quiz_question4_id
  )
  option2 = OptionCreate(
      text="compile(useless).",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option3 = OptionCreate(
      text="compile:file(useless).",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option4 = OptionCreate(
      text="c(useless, compile).",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 5
  quiz_question5 = QuizQuestionCreate(
      question_text="¿Cuál de las siguientes afirmaciones sobre funciones dentro de un módulo es correcta?",
      quiz_id=quiz5_id
  )
  quiz_question5_db = await quiz_crud.create_quiz_question(db, quiz_question5)
  quiz_question5_id = quiz_question5_db.id

  option1 = OptionCreate(
      text="Las funciones pueden llamarse entre sí dentro del mismo módulo sin anteponer el nombre del módulo.",
      is_correct=True,
      quiz_question_id=quiz_question5_id
  )
  option2 = OptionCreate(
      text="Las funciones deben llamarse siempre con el nombre del módulo, incluso dentro del mismo módulo.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option3 = OptionCreate(
      text="No se pueden importar funciones de otros módulos.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option4 = OptionCreate(
      text="Las funciones dentro de un módulo no pueden contener comentarios.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Crear el quiz 6
  quiz6 = QuizCreate(
      title="Quiz Erlang: Expresiones Avanzadas y Pattern Matching",
      description="Quiz sobre pattern matching, guardas, if y manejo de listas y tuplas en Erlang.",
      subunit_id=subunit1_6_db.id
  )
  quiz6_db = await quiz_crud.create_quiz(db, quiz6)
  quiz6_id = quiz6_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="¿Qué ventaja principal ofrece el Pattern Matching en funciones sobre un 'if' tradicional?",
      quiz_id=quiz6_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  option1 = OptionCreate(
      text="Simplifica el código evitando escribir múltiples condiciones repetitivas.",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  )
  option2 = OptionCreate(
      text="Hace que las funciones se ejecuten más rápido que el bytecode compilado.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option3 = OptionCreate(
      text="Permite declarar variables globales automáticamente.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option4 = OptionCreate(
      text="Elimina la necesidad de módulos.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="En el módulo greet, ¿qué cláusula maneja todos los casos que no sean 'male' o 'female'?",
      quiz_id=quiz6_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  option1 = OptionCreate(
      text="greet(_, Name) -> ...",
      is_correct=True,
      quiz_question_id=quiz_question2_id
  )
  option2 = OptionCreate(
      text="greet(other, Name) -> ...",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option3 = OptionCreate(
      text="greet(Name) -> ...",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option4 = OptionCreate(
      text="greet(Name, Gender) -> ...",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="¿Cómo se obtiene el segundo elemento de una lista usando pattern matching?",
      quiz_id=quiz6_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  option1 = OptionCreate(
      text="second([_, X | _]) -> X.",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  )
  option2 = OptionCreate(
      text="second([X, _ | _]) -> X.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option3 = OptionCreate(
      text="second([X | _]) -> X.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option4 = OptionCreate(
      text="second([_, _]) -> X.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="En la función valid_time({Date = {Y,M,D}, Time = {H,Min,S}}), ¿qué permite el operador '=' en la cabecera?",
      quiz_id=quiz6_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  option1 = OptionCreate(
      text="Hacer coincidir tanto el contenido interno de la tupla como la tupla completa.",
      is_correct=True,
      quiz_question_id=quiz_question4_id
  )
  option2 = OptionCreate(
      text="Asignar valores nuevos a las variables sin importar la tupla original.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option3 = OptionCreate(
      text="Comparar la tupla con listas automáticamente.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option4 = OptionCreate(
      text="Evitar errores de compilación cuando la tupla está vacía.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 5
  quiz_question5 = QuizQuestionCreate(
      question_text="¿Cuál es la diferencia principal entre guardas y if en Erlang?",
      quiz_id=quiz6_id
  )
  quiz_question5_db = await quiz_crud.create_quiz_question(db, quiz_question5)
  quiz_question5_id = quiz_question5_db.id

  option1 = OptionCreate(
      text="Las guardas van en la cabecera de la función; if se usa dentro del cuerpo de la función.",
      is_correct=True,
      quiz_question_id=quiz_question5_id
  )
  option2 = OptionCreate(
      text="Las guardas solo funcionan con listas, while if funciona con tuplas.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option3 = OptionCreate(
      text="If permite rangos de valores, guardas no.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option4 = OptionCreate(
      text="No hay diferencias, solo es cuestión de estilo.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Crear el quiz 7
  quiz7 = QuizCreate(
      title="Quiz Erlang: Funciones de Orden Superior y Funciones Anónimas",
      description="Preguntas sobre funciones de orden superior, funciones anónimas, closures y recursión anónima en Erlang.",
      subunit_id=subunit2_1_db.id
  )
  quiz7_db = await quiz_crud.create_quiz(db, quiz7)
  quiz7_id = quiz7_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="¿Qué caracteriza a una función de orden superior?",
      quiz_id=quiz7_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  option1 = OptionCreate(
      text="Puede recibir funciones como parámetros o devolver funciones como resultado.",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  )
  option2 = OptionCreate(
      text="Siempre es recursiva.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option3 = OptionCreate(
      text="Debe estar definida como anónima.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option4 = OptionCreate(
      text="Solo puede operar con listas.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="Para pasar una función como argumento en Erlang, ¿qué sintaxis se usa?",
      quiz_id=quiz7_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  option1 = OptionCreate(
      text="fun Module:Function/Arity",
      is_correct=True,
      quiz_question_id=quiz_question2_id
  )
  option2 = OptionCreate(
      text="Module.Function()",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option3 = OptionCreate(
      text="Function()",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option4 = OptionCreate(
      text="fun Function()",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="¿Cuál es la ventaja de usar funciones anónimas (funs) en Erlang?",
      quiz_id=quiz7_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  option1 = OptionCreate(
      text="Evitan declarar y exportar funciones solo para usarlas una vez.",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  )
  option2 = OptionCreate(
      text="Hacen que las funciones sean más rápidas.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option3 = OptionCreate(
      text="Permiten cambiar variables globales automáticamente.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option4 = OptionCreate(
      text="Siempre permiten recursión infinita sin bucles.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="¿Qué es un closure en Erlang?",
      quiz_id=quiz7_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  option1 = OptionCreate(
      text="Una función anónima que recuerda variables externas de su entorno.",
      is_correct=True,
      quiz_question_id=quiz_question4_id
  )
  option2 = OptionCreate(
      text="Una función recursiva sin nombre.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option3 = OptionCreate(
      text="Una función que solo opera sobre listas.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option4 = OptionCreate(
      text="Una función exportada que devuelve otra función.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 5
  quiz_question5 = QuizQuestionCreate(
      question_text="¿Cómo se logra la recursión dentro de una función anónima desde Erlang 17?",
      quiz_id=quiz7_id
  )
  quiz_question5_db = await quiz_crud.create_quiz_question(db, quiz_question5)
  quiz_question5_id = quiz_question5_db.id

  option1 = OptionCreate(
      text="Dándole un nombre interno a la función anónima y llamándose a sí misma.",
      is_correct=True,
      quiz_question_id=quiz_question5_id
  )
  option2 = OptionCreate(
      text="Usando map/2 con la misma función.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option3 = OptionCreate(
      text="Solo pasando la función como parámetro de otra función.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option4 = OptionCreate(
      text="No es posible hacer recursión en funciones anónimas.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Crear el quiz 8
  quiz8 = QuizCreate(
      title="Quiz Erlang: Recursión y Recursión de Cola",
      description="Preguntas sobre recursión, recursión de cola, acumuladores y patrones recursivos en Erlang.",
      subunit_id=subunit2_2_db.id
  )
  quiz8_db = await quiz_crud.create_quiz(db, quiz8)
  quiz8_id = quiz8_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="¿Qué es la recursión en Erlang?",
      quiz_id=quiz8_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  option1 = OptionCreate(
      text="Una función que se llama a sí misma hasta alcanzar un caso base.",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  )
  option2 = OptionCreate(
      text="Un bucle for o while clásico.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option3 = OptionCreate(
      text="Una estructura de datos especial.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option4 = OptionCreate(
      text="Una función que siempre devuelve listas.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="¿Qué distingue a la recursión de cola (tail recursion)?",
      quiz_id=quiz8_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  option1 = OptionCreate(
      text="El resultado de la llamada recursiva se devuelve directamente, permitiendo optimización de memoria.",
      is_correct=True,
      quiz_question_id=quiz_question2_id
  )
  option2 = OptionCreate(
      text="No necesita caso base.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option3 = OptionCreate(
      text="Siempre usa listas vacías como acumuladores.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option4 = OptionCreate(
      text="No se puede usar con funciones matemáticas.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="¿Cuál es la función del 'caso base' en la recursión?",
      quiz_id=quiz8_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  option1 = OptionCreate(
      text="Detener la recursión para evitar llamadas infinitas.",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  )
  option2 = OptionCreate(
      text="Aumentar la profundidad de la recursión.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option3 = OptionCreate(
      text="Multiplicar el resultado final.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option4 = OptionCreate(
      text="Permitir el uso de funciones anónimas.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="¿Por qué se usan acumuladores en recursión de cola?",
      quiz_id=quiz8_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  option1 = OptionCreate(
      text="Para mantener el estado y permitir optimización de memoria.",
      is_correct=True,
      quiz_question_id=quiz_question4_id
  )
  option2 = OptionCreate(
      text="Para definir el caso base.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option3 = OptionCreate(
      text="Para crear listas vacías automáticamente.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option4 = OptionCreate(
      text="Para evitar usar pattern matching.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 5
  quiz_question5 = QuizQuestionCreate(
      question_text="¿Cuál es un patrón clave para pensar recursivamente?",
      quiz_id=quiz8_id
  )
  quiz_question5_db = await quiz_crud.create_quiz_question(db, quiz_question5)
  quiz_question5_id = quiz_question5_db.id

  option1 = OptionCreate(
      text="Identificar el caso base y reducir el problema hacia él en cada llamada.",
      is_correct=True,
      quiz_question_id=quiz_question5_id
  )
  option2 = OptionCreate(
      text="Evitar usar listas y solo trabajar con números.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option3 = OptionCreate(
      text="Usar bucles for y while siempre.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option4 = OptionCreate(
      text="Definir todas las funciones como anónimas.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Crear el quiz 9
  quiz9 = QuizCreate(
      title="Quiz Erlang: Estructuras de Datos",
      description="Preguntas sobre records, almacenamiento clave/valor y conjuntos en Erlang.",
      subunit_id=subunit2_3_db.id  # Ajusta al subunit correspondiente
  )
  quiz9_db = await quiz_crud.create_quiz(db, quiz9)
  quiz9_id = quiz9_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="Dado el record #robot{name, type = industrial, hobbies, details = []}, si escribimos R = #robot{name=\"RoboCop\"}, ¿cuál será el valor de R#robot.type y R#robot.details?",
      quiz_id=quiz9_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="industrial y []",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text='"RoboCop" y []',
      is_correct=False,
      quiz_question_id=quiz_question1_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text='industrial y undefined',
      is_correct=False,
      quiz_question_id=quiz_question1_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text='undefined y []',
      is_correct=False,
      quiz_question_id=quiz_question1_id
  ))

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="Cuál es la salida de admin_panel(#user{name=\"Alice\", group=admin}) usando pattern matching en records?",
      quiz_id=quiz9_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text='"Alice is allowed!"',
      is_correct=True,
      quiz_question_id=quiz_question2_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text='"Alice is not allowed"',
      is_correct=False,
      quiz_question_id=quiz_question2_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text='Error de compilación',
      is_correct=False,
      quiz_question_id=quiz_question2_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text='"admin is allowed!"',
      is_correct=False,
      quiz_question_id=quiz_question2_id
  ))

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="Si tenemos un record anidado NestedBot = #robot{details=#robot{name=\"erNest\"}}, ¿cómo accedemos al name del robot interno?",
      quiz_id=quiz9_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="NestedBot#robot.details#robot.name",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="NestedBot.details.name",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="NestedBot#robot.details.name",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="NestedBot.name.details",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  ))

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="¿Cuál estructura es más recomendable para datos pequeños y con pocas claves?",
      quiz_id=quiz9_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="proplists",
      is_correct=True,
      quiz_question_id=quiz_question4_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="dict",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="sets",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="orddict",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  ))

  # Pregunta 5
  quiz_question5 = QuizQuestionCreate(
      question_text="Dada D = dict:store(user, \"Alice\", dict:new()), dict:find(user, D), ¿qué devuelve?",
      quiz_id=quiz9_id
  )
  quiz_question5_db = await quiz_crud.create_quiz_question(db, quiz_question5)
  quiz_question5_id = quiz_question5_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="{ok, \"Alice\"}",
      is_correct=True,
      quiz_question_id=quiz_question5_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="ok",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text='"Alice"',
      is_correct=False,
      quiz_question_id=quiz_question5_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text='{"Alice"}',
      is_correct=False,
      quiz_question_id=quiz_question5_id
  ))

  # Pregunta 6
  quiz_question6 = QuizQuestionCreate(
      question_text="¿Cuál es la diferencia principal entre ordsets y sets?",
      quiz_id=quiz9_id
  )
  quiz_question6_db = await quiz_crud.create_quiz_question(db, quiz_question6)
  quiz_question6_id = quiz_question6_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="sets usa =:= para comparar, ordsets está ordenado",
      is_correct=True,
      quiz_question_id=quiz_question6_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="ordsets permite duplicados",
      is_correct=False,
      quiz_question_id=quiz_question6_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="sets es más lento que ordsets",
      is_correct=False,
      quiz_question_id=quiz_question6_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="Ninguna, funcionan igual",
      is_correct=False,
      quiz_question_id=quiz_question6_id
  ))

  # Pregunta 7
  quiz_question7 = QuizQuestionCreate(
      question_text="Si queremos un conjunto grande con comparación estricta entre elementos, ¿qué estructura usamos?",
      quiz_id=quiz9_id
  )
  quiz_question7_db = await quiz_crud.create_quiz_question(db, quiz_question7)
  quiz_question7_id = quiz_question7_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="sets",
      is_correct=True,
      quiz_question_id=quiz_question7_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="ordsets",
      is_correct=False,
      quiz_question_id=quiz_question7_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="proplists",
      is_correct=False,
      quiz_question_id=quiz_question7_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="dict",
      is_correct=False,
      quiz_question_id=quiz_question7_id
  ))

  # Crear el quiz 10
  quiz10 = QuizCreate(
      title="Quiz Erlang: Errores y Excepciones",
      description="Preguntas sobre tipos de errores, excepciones y manejo de errores en Erlang.",
      subunit_id=subunit2_4_db.id  # Ajusta al subunit correspondiente
  )
  quiz10_db = await quiz_crud.create_quiz(db, quiz10)
  quiz10_id = quiz10_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="¿Cuál de estos errores se detecta en tiempo de compilación?",
      quiz_id=quiz10_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="Module name 'madule' does not match file name 'module'",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="function_clause",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="badmatch",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="exit",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  ))

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="¿Qué tipo de error ocurre cuando un pattern matching falla en tiempo de ejecución?",
      quiz_id=quiz10_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="badmatch",
      is_correct=True,
      quiz_question_id=quiz_question2_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="case_clause",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="syntax error",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="throw",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  ))

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="En Erlang, cuál es la función de exit(Reason)?",
      quiz_id=quiz10_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="Termina el proceso actual o externo sin stack trace",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="Control de flujo similar a return",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="Genera error de compilación",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="Lanza un error con stack trace",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  ))

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="Qué bloque se ejecuta siempre, ocurra o no una excepción?",
      quiz_id=quiz10_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="after",
      is_correct=True,
      quiz_question_id=quiz_question4_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="catch",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="throw",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="exit",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  ))

  # Pregunta 5
  quiz_question5 = QuizQuestionCreate(
      question_text="Cuál es la forma recomendada de manejar errores en Erlang moderno?",
      quiz_id=quiz10_id
  )
  quiz_question5_db = await quiz_crud.create_quiz_question(db, quiz_question5)
  quiz_question5_id = quiz_question5_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="try ... catch",
      is_correct=True,
      quiz_question_id=quiz_question5_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="catch (forma antigua)",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="throw sin try",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="Ignorar errores",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  ))

  # Pregunta 6
  quiz_question6 = QuizQuestionCreate(
      question_text="Qué tipo de excepción usarías para control de flujo que el llamador debe capturar?",
      quiz_id=quiz10_id
  )
  quiz_question6_db = await quiz_crud.create_quiz_question(db, quiz_question6)
  quiz_question6_id = quiz_question6_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="throw",
      is_correct=True,
      quiz_question_id=quiz_question6_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="exit",
      is_correct=False,
      quiz_question_id=quiz_question6_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="error",
      is_correct=False,
      quiz_question_id=quiz_question6_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="badmatch",
      is_correct=False,
      quiz_question_id=quiz_question6_id
  ))

  # Pregunta 7
  quiz_question7 = QuizQuestionCreate(
      question_text="Qué práctica se recomienda en Erlang según la filosofía “let it crash”?",
      quiz_id=quiz10_id
  )
  quiz_question7_db = await quiz_crud.create_quiz_question(db, quiz_question7)
  quiz_question7_id = quiz_question7_db.id

  await quiz_crud.create_option(db, OptionCreate(
      text="Dejar que un proceso falle y que otro lo supervise",
      is_correct=True,
      quiz_question_id=quiz_question7_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="Atrapar todos los errores manualmente",
      is_correct=False,
      quiz_question_id=quiz_question7_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="Usar catch en todas las funciones",
      is_correct=False,
      quiz_question_id=quiz_question7_id
  ))
  await quiz_crud.create_option(db, OptionCreate(
      text="Evitar throw en todo el código",
      is_correct=False,
      quiz_question_id=quiz_question7_id
  ))

  

  # Crear el quiz 11
  quiz11 = QuizCreate(
      title="Quiz Erlang: Procesos y Concurrencia",
      description="Preguntas sobre concurrencia, procesos, paso de mensajes y primitivas en Erlang.",
      subunit_id=subunit3_1_db.id
  )
  quiz11_db = await quiz_crud.create_quiz(db, quiz11)
  quiz11_id = quiz11_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="¿Cuál es la diferencia entre concurrencia y paralelismo en Erlang?",
      quiz_id=quiz11_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  option1 = OptionCreate(
      text="La concurrencia es ejecutar múltiples procesos de forma independiente, mientras que el paralelismo es ejecutarlos exactamente al mismo tiempo en distintos núcleos.",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  )
  option2 = OptionCreate(
      text="La concurrencia siempre usa varios núcleos, el paralelismo no.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option3 = OptionCreate(
      text="El paralelismo solo existe en Erlang, la concurrencia en otros lenguajes.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option4 = OptionCreate(
      text="Son lo mismo, solo cambian los términos.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="¿Qué primitivas básicas existen en Erlang para manejar procesos?",
      quiz_id=quiz11_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  option1 = OptionCreate(
      text="spawn/3, self(), '!' (envío de mensajes), receive, flush().",
      is_correct=True,
      quiz_question_id=quiz_question2_id
  )
  option2 = OptionCreate(
      text="for, while, goto, break, continue.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option3 = OptionCreate(
      text="new Thread(), run(), sleep(), join().",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option4 = OptionCreate(
      text="malloc(), free(), pthread_create().",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="¿Cómo se comunican los procesos en Erlang?",
      quiz_id=quiz11_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  option1 = OptionCreate(
      text="Mediante paso de mensajes entre mailboxes.",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  )
  option2 = OptionCreate(
      text="Compartiendo memoria directamente.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option3 = OptionCreate(
      text="A través de punteros globales.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option4 = OptionCreate(
      text="Por medio de sockets obligatoriamente.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Crear el quiz 12
  quiz12 = QuizCreate(
      title="Quiz Erlang: Comunicación entre Procesos",
      description="Preguntas sobre envío y recepción de mensajes, procesos con estado, timeouts y recepción selectiva en Erlang.",
      subunit_id=subunit3_2_db.id
  )
  quiz12_db = await quiz_crud.create_quiz(db, quiz12)
  quiz12_id = quiz12_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="¿Cómo mantienen los procesos en Erlang su estado interno?",
      quiz_id=quiz12_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  option1 = OptionCreate(
      text="Mediante recursión, pasando el estado actualizado como argumento en cada llamada.",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  )
  option2 = OptionCreate(
      text="Usando variables globales compartidas entre procesos.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option3 = OptionCreate(
      text="Guardando el estado en archivos temporales.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option4 = OptionCreate(
      text="A través de registros estáticos en el compilador.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="¿Qué ventaja ofrecen las funciones de interfaz como store/2 y take/2 en el módulo kitchen?",
      quiz_id=quiz12_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  option1 = OptionCreate(
      text="Ocultan la complejidad de los mensajes, simplificando la interacción con el proceso.",
      is_correct=True,
      quiz_question_id=quiz_question2_id
  )
  option2 = OptionCreate(
      text="Permiten compartir memoria entre procesos.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option3 = OptionCreate(
      text="Compilan el código en modo nativo automáticamente.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option4 = OptionCreate(
      text="Eliminan la necesidad de usar spawn/3.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="¿Qué ocurre si un proceso no responde y no se usa un timeout en la recepción?",
      quiz_id=quiz12_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  option1 = OptionCreate(
      text="El shell puede quedarse bloqueado indefinidamente esperando el mensaje.",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  )
  option2 = OptionCreate(
      text="El proceso se reinicia automáticamente después de 3 segundos.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option3 = OptionCreate(
      text="Se lanza una excepción de tipo 'timeout_error'.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option4 = OptionCreate(
      text="El mensaje se descarta silenciosamente.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="¿Para qué sirve la recepción selectiva en Erlang?",
      quiz_id=quiz12_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  option1 = OptionCreate(
      text="Permite dar prioridad a ciertos mensajes sobre otros.",
      is_correct=True,
      quiz_question_id=quiz_question4_id
  )
  option2 = OptionCreate(
      text="Sirve para descartar automáticamente todos los mensajes de baja prioridad.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option3 = OptionCreate(
      text="Convierte los mensajes en llamadas síncronas.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option4 = OptionCreate(
      text="Hace que los procesos compartan memoria de forma eficiente.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Crear el quiz 13
  quiz13 = QuizCreate(
      title="Quiz Erlang: Monitoreo y Manejo de Fallos",
      description="Preguntas sobre links, monitores, trap_exit, supervisores, procesos nombrados y referencias únicas en Erlang.",
      subunit_id=subunit3_3_db.id
  )
  quiz13_db = await quiz_crud.create_quiz(db, quiz13)
  quiz13_id = quiz13_db.id

  # Pregunta 1
  quiz_question1 = QuizQuestionCreate(
      question_text="¿Qué ocurre cuando dos procesos están enlazados (link) y uno de ellos muere inesperadamente?",
      quiz_id=quiz13_id
  )
  quiz_question1_db = await quiz_crud.create_quiz_question(db, quiz_question1)
  quiz_question1_id = quiz_question1_db.id

  option1 = OptionCreate(
      text="El otro proceso también muere propagando el fallo.",
      is_correct=True,
      quiz_question_id=quiz_question1_id
  )
  option2 = OptionCreate(
      text="El proceso sobreviviente recibe un valor null y sigue ejecutándose.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option3 = OptionCreate(
      text="No pasa nada, los procesos siguen independientes.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  option4 = OptionCreate(
      text="El proceso enlazado se reinicia automáticamente.",
      is_correct=False,
      quiz_question_id=quiz_question1_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 2
  quiz_question2 = QuizQuestionCreate(
      question_text="¿Qué hace la función spawn_link(Function)?",
      quiz_id=quiz13_id
  )
  quiz_question2_db = await quiz_crud.create_quiz_question(db, quiz_question2)
  quiz_question2_id = quiz_question2_db.id

  option1 = OptionCreate(
      text="Crea un proceso y lo enlaza en una sola operación atómica.",
      is_correct=True,
      quiz_question_id=quiz_question2_id
  )
  option2 = OptionCreate(
      text="Crea un proceso sin relación con el actual.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option3 = OptionCreate(
      text="Reinicia automáticamente procesos caídos.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  option4 = OptionCreate(
      text="Monitorea un proceso sin matarlo si falla.",
      is_correct=False,
      quiz_question_id=quiz_question2_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 3
  quiz_question3 = QuizQuestionCreate(
      question_text="¿Qué permite process_flag(trap_exit, true)?",
      quiz_id=quiz13_id
  )
  quiz_question3_db = await quiz_crud.create_quiz_question(db, quiz_question3)
  quiz_question3_id = quiz_question3_db.id

  option1 = OptionCreate(
      text="Convertir las señales de salida en mensajes que el proceso puede manejar.",
      is_correct=True,
      quiz_question_id=quiz_question3_id
  )
  option2 = OptionCreate(
      text="Ignorar permanentemente los fallos de otros procesos.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option3 = OptionCreate(
      text="Reiniciar automáticamente al proceso si muere.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  option4 = OptionCreate(
      text="Crear un proceso monitor automáticamente.",
      is_correct=False,
      quiz_question_id=quiz_question3_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 4
  quiz_question4 = QuizQuestionCreate(
      question_text="¿Qué diferencia principal tienen los monitores respecto a los links?",
      quiz_id=quiz13_id
  )
  quiz_question4_db = await quiz_crud.create_quiz_question(db, quiz_question4)
  quiz_question4_id = quiz_question4_db.id

  option1 = OptionCreate(
      text="Los monitores son unidireccionales y apilables, mientras que los links son bidireccionales.",
      is_correct=True,
      quiz_question_id=quiz_question4_id
  )
  option2 = OptionCreate(
      text="Los monitores reinician procesos automáticamente al morir.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option3 = OptionCreate(
      text="Los monitores convierten fallos en señales de log.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  option4 = OptionCreate(
      text="Los monitores solo existen en OTP, no en Erlang puro.",
      is_correct=False,
      quiz_question_id=quiz_question4_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Pregunta 5
  quiz_question5 = QuizQuestionCreate(
      question_text="¿Cómo se evita una race condition al usar procesos nombrados con register/2?",
      quiz_id=quiz13_id
  )
  quiz_question5_db = await quiz_crud.create_quiz_question(db, quiz_question5)
  quiz_question5_id = quiz_question5_db.id

  option1 = OptionCreate(
      text="Usando referencias únicas (make_ref) en los mensajes para identificarlos de forma segura.",
      is_correct=True,
      quiz_question_id=quiz_question5_id
  )
  option2 = OptionCreate(
      text="Reiniciando manualmente el proceso después de cada fallo.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option3 = OptionCreate(
      text="Asignando múltiples nombres al mismo proceso.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  option4 = OptionCreate(
      text="Deshabilitando el uso de register/2 en procesos críticos.",
      is_correct=False,
      quiz_question_id=quiz_question5_id
  )
  await quiz_crud.create_option(db, option1)
  await quiz_crud.create_option(db, option2)
  await quiz_crud.create_option(db, option3)
  await quiz_crud.create_option(db, option4)

  # Question 1
  question1 = QuestionCreate(
      title="How do I print in Erlang?",
      body="What is the function to print text in Erlang?",
      tags=["erlang", "print"],
      user_id=1
  )
  question1_db = await publication_crud.create_question(db, question1)
  question1_id = question1_db.id

  answer1 = AnswerCreate(
      body='Use io:format("Text~n").',
      question_id=question1_id,
      user_id=2
  )
  await publication_crud.create_answer(db, answer1)

  vote1 = VoteCreate(
      user_id=3,
      question_id=question1_id,
      vote='upvote'
  )
  await vote_crud.create_vote(db, vote1)

  # Question 2
  question2 = QuestionCreate(
      title="How to define a module in Erlang?",
      body="What is the syntax to define a module?",
      tags=["erlang", "module"],
      user_id=2
  )
  question2_db = await publication_crud.create_question(db, question2)
  question2_id = question2_db.id

  answer2 = AnswerCreate(
      body='Use -module(module_name). at the top of your file.',
      question_id=question2_id,
      user_id=1
  )
  await publication_crud.create_answer(db, answer2)

  vote2 = VoteCreate(
      user_id=3,
      question_id=question2_id,
      vote='upvote'
  )
  await vote_crud.create_vote(db, vote2)

  # Question 3
  question3 = QuestionCreate(
      title="How to export functions in Erlang?",
      body="How do I make a function public in Erlang?",
      tags=["erlang", "export"],
      user_id=3
  )
  question3_db = await publication_crud.create_question(db, question3)
  question3_id = question3_db.id

  answer3 = AnswerCreate(
      body='Use -export([function_name/arity]).',
      question_id=question3_id,
      user_id=1
  )
  await publication_crud.create_answer(db, answer3)

  answer3b = AnswerCreate(
      body='For example: -export([start/0, add/2]).',
      question_id=question3_id,
      user_id=2
  )
  await publication_crud.create_answer(db, answer3b)

  vote3 = VoteCreate(
      user_id=1,
      question_id=question3_id,
      vote='upvote'
  )
  await vote_crud.create_vote(db, vote3)

  vote3b = VoteCreate(
      user_id=2,
      question_id=question3_id,
      vote='upvote'
  )
  await vote_crud.create_vote(db, vote3b)


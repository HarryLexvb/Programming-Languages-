defmodule Menu do
  def display_menu do
    IO.puts("=====================================")
    IO.puts("|           Pixel Arcade            |")
    IO.puts("=====================================")
    IO.puts("| 1. Abrir Snake                    |")
    IO.puts("| 2. Abrir El ahorcado              |")
    IO.puts("| 3. Abrir 3 en raya                |")
    IO.puts("| 0. Salir                          |")
    IO.puts("=====================================")
    IO.puts("Ingrese una opción:")
    input = IO.gets("") |> String.trim

    case input do
      "1" -> open_html_file("/home/harrylex/Documents/UCSP/snake/index.html")
      "2" -> open_html_file("/home/harrylex/Documents/UCSP/ahorcado/index.html")
      "3" -> open_html_file("/home/harrylex/Documents/UCSP/3enraya/index.html")
      "0" -> IO.puts("Saliendo del menú...")
      _ -> IO.puts("Opción inválida, por favor intente de nuevo.")
    end

    unless input == "0" do
      display_menu()
    end
  end

  def open_html_file(filename) do
    IO.puts("Abriendo el archivo #{filename}...")
    System.cmd("brave", [filename])
  end
end

Menu.display_menu()

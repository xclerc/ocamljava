open Package'java'awt
open Package'java'awt'event
open Package'javax'swing
    
let () =
  let str = JavaString.of_string in
  let open Java in
  let title = str "Celsius Converter" in
  let frame = make "JFrame(String)" title in
  let temp_text = make "JTextField()" () in
  let celsius_label = make "JLabel(String)" (str "Celsius") in
  let convert_button = make "JButton(String)" (str "Convert") in
  let farenheit_label = make "JLabel(String)" (str "Farenheit") in
  let handler = proxy "ActionListener" (object
    method actionPerformed _ =
      try
        let c = call "JTextField.getText()" temp_text in
        let c = call "Double.parseDouble(_)" c in
        let f = (c *. 1.8) +. 32.0 in
        let f = Printf.sprintf "%f Farenheit" f in
        call "JLabel.setText(_)" farenheit_label (str f)
      with Java_exception je ->
        let je_msg = call "Throwable.getMessage()" je in
        let je_msg = JavaString.to_string je_msg in
        let msg = str (Printf.sprintf "invalid float value (%s)" je_msg) in
        let error = get "JOptionPane.ERROR_MESSAGE" () in
        call "JOptionPane.showMessageDialog(_,_,_,_)"
          frame msg title error
  end) in
  let () = call "JButton.addActionListener(_)" convert_button handler in
  let layout = make "GridLayout(_,_,_,_)" 2l 2l 3l 3l in
  call "JFrame.setLayout(_)" frame layout;
  ignore (call "JFrame.add(Component)" frame temp_text);
  ignore (call "JFrame.add(Component)" frame celsius_label);
  ignore (call "JFrame.add(Component)" frame convert_button);
  ignore (call "JFrame.add(Component)" frame farenheit_label);
  call "JFrame.setSize(_,_)" frame 300l 80l;
  let exit = get "WindowConstants.EXIT_ON_CLOSE" () in
  call "JFrame.setDefaultCloseOperation(int)" frame exit;
  call "JFrame.setVisible(_)" frame true

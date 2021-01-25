/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package chatbot;

import javax.swing.*;
import java.awt.event.*;


class TestDialog extends JFrame implements ActionListener {
   public TestDialog(String title) {
      super(title);
      setBounds(0,0,600,400);
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      JMenuBar menuBar = new JMenuBar();
      setJMenuBar(menuBar);
      JMenu file = new JMenu("File");
      JMenuItem quit = new JMenuItem("Quit");
      quit.addActionListener(this);
      file.add(quit);
      menuBar.add(file);
      JMenu data = new JMenu("Data");
      JMenuItem enter = new JMenuItem("Enter data");
      enter.addActionListener(this);
      data.add(enter);
      menuBar.add(data);
   }
   public void actionPerformed(ActionEvent ae) {
      String choice = ae.getActionCommand();
      if (choice.equals("Quit")) {
         System.exit(0);
      }
      else if (choice.equals("Enter data")) {
         MyDialog dlg = new MyDialog(this);
         String[] results = dlg.run();
         if (results[0] != null) {
            JOptionPane.showMessageDialog(this,
               results[0] + ", color: " + results[1]);
         }
      }
   }
   public static void main(String[] args) {
      TestDialog myApp = new TestDialog("Test Dialog");
      myApp.setVisible(true);
   }
}
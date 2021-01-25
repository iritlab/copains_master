/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package chatbot;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import static java.lang.System.console;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
public class MyDialog extends JDialog implements ActionListener {
   private String[] data;
   private JTextField descBox;
   private JComboBox<String> colorList;
   private JList listval;
   private JButton btnOk;
   private JButton btnCancel;
   private JCheckBox checkbox;
   
   
   
   
   
   public MyDialog(Frame parent) {
       
      super(parent,"Humans' actual desires",true);
      Point loc = parent.getLocation();
      setLocation(loc.x+80,loc.y+80);
      
      String[] colorStrings = {"env","loc","cost"};
      
      String [] items1 = { "land", "water" }; 
      String [] items2 = { "indoor", "outdoor", "mixed"};       
      
      data = new String[2]; // set to amount of data items
      JPanel panel = new JPanel();
      panel.setLayout(new GridBagLayout());
      GridBagConstraints gbc = new GridBagConstraints();
      gbc.insets = new Insets(5,5,5,5);
  
      
      
      
      JLabel colorLabel = new JLabel("Variable :");
      gbc.gridwidth = 1;
      gbc.gridx = 0;
      gbc.gridy = 1;
      panel.add(colorLabel,gbc);
      
      colorList = new JComboBox<String>(colorStrings);
      colorList.addActionListener(this);
      gbc.gridwidth = 1;
      gbc.gridx = 1;
      gbc.gridy = 1;
      panel.add(colorList,gbc);
      
      
      JLabel valueLabel = new JLabel("Value :");
      gbc.gridwidth = 1;
      gbc.gridx = 2;
      gbc.gridy = 1;
      panel.add(valueLabel,gbc);   
      
      checkbox = new JCheckBox("Not"); 
      checkbox.setSelected(false);      
      gbc.gridwidth = 1;
      gbc.gridx = 3;
      gbc.gridy = 1;
      panel.add(checkbox,gbc);        
     
      listval = new JList(items1);
      gbc.gridx = 4;
      gbc.gridy = 1;
      panel.add(listval,gbc);      
      
      JLabel spacer = new JLabel(" ");
      gbc.gridx = 0;
      gbc.gridy = 2;
      panel.add(spacer,gbc);

      
      
      btnOk = new JButton("Ok");
      btnOk.addActionListener(this);
      gbc.gridwidth = 1;
      gbc.gridx = 4;
      gbc.gridy = 4;
      panel.add(btnOk,gbc);
      
      btnCancel = new JButton("Cancel");
      btnCancel.addActionListener(this);
      gbc.gridx = 3;
      gbc.gridy = 4;
      panel.add(btnCancel,gbc);

      
      getContentPane().add(panel);
      pack();
   }
   
   public void actionPerformed(ActionEvent ae) {
      Object source = ae.getSource();
      
      String [] items1 = { "land", "water" }; 
      String [] items2 = { "indoor", "outdoor", "mixed"};       
      
      if (source == colorList) {
          String svar;
          svar = (String)colorList.getSelectedItem();
          System.out.println("Entro al "+svar);
          switch (svar) {
                        case "env":
                            listval.setListData(items1);
                            System.out.println("Entro al env");
                            break;
                        case "loc":
                            listval.setListData(items2);
                            System.out.println("Entro al loc");
                            break;          
          }
      } 
      
      
      if (source == btnOk) {
         /*data[0] = descBox.getText();*/
         /*data[0] = list.getSelectedValue();*/
         
         data[1] = (String)colorList.getSelectedItem();
         
        if (checkbox.isSelected()) {
            data[0] = "not "+(String)listval.getSelectedValue();
        } else {
             data[0] = (String)listval.getSelectedValue();
         }
         
         
         dispose();
      }
      else {
         data[0] = null;
      }
      /*dispose();*/
   }
   public String[] run() {
      this.setVisible(true);
      return data;
   }
}
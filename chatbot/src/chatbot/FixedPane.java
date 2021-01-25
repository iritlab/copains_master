/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package chatbot;

import java.awt.event.*;
import javax.swing.*;
import java.awt.*;
  
public class FixedPane extends JFrame
{
    JTextPane tp;
  
    public FixedPane()
    {
        tp = new JTextPane();
        JPanel p = new JPanel();
        tp.setBounds( 0, 0, 200, 200 );
        p.setLayout( null );
        p.add( tp );
        p.setPreferredSize( new Dimension( 200, 200 ) );
        getContentPane().add( p, BorderLayout.CENTER );
        addWindowListener( new WindowAdapter() {
            public void windowClosing( WindowEvent event )
            {
                System.exit( 0 );
            }
        } );
        pack();
        setVisible( true );
    }
  
    public static void main( String args[] )
    {
        new FixedPane();
    }
}
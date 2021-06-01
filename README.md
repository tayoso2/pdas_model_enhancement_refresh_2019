# PDaS and S24 Spatial Mapping Project

## Project Description

The PDaS and S24 Spatial Mapping project was designed to utilise advanced spatial analytics in  improving STW’s understanding of the location of S24 shared drains and PDaS assets and their key  attributes, i.e., length, material, diameter and yearlaid. Arcadis Gen was tasked with generating a set of  inferred PDaS and S24 assets and their neighbouring proxy sewer mains.



## Resources

**Final Report**

https://arcadiso365.sharepoint.com/teams/PDaSAssetBaseCreation/Shared%20Documents/General/Presentations/Phase%202/Final%20Report/PDaS%20and%20S24%20Spatial%20Mapping%20Final%20Report%20v2.pdf

**Phase 2 Decision Log**

https://arcadiso365.sharepoint.com/teams/PDaSAssetBaseCreation/_layouts/15/Doc.aspx?OR=teams&action=edit&sourcedoc={AEFC1F8B-5C09-4B58-A119-C08EA4BBEAE1}





## Role

- Tayo Ososanya - Spatial Analyst/Project Manager
- Will Bailey - Spatial Analyst
- Shuyao Chen - Spatial Analyst
- Nik Humphries - Spatial Analyst
- Simone Croft - Project Manager
- Carl Takamizawa - Project Manager

 

## Contacts 

- Will Bailey - Analytics Consultant
- Shuyao Chen - Analytics Consultant
- Carl Takamizawa - Associate Director of Analytics



**File-location**

It is located on the SSD 

> D:\STW PDAS\



## Scripts

The scripts used are on www.gitlab.com/tayoso2/pdas_mapping. 

### Proxy mains

**Attribution**

1. ./proxy sewer analysis/Will_proxy_attribute_assigning_progress/offset proxies_wb_edit.r

2. ./proxy sewer analysis/assign proxy sewer atts.r

*Assign proxy sewer atts was created using the prediction models from ML proxy sewer atts.*

**Flow Direction** 

1. Data from Attribution
2. ./pipe-depth-from-lidar-images/scripts/pipe_lidar.r
3. ./pipe-depth-from-lidar-images/scripts/pipe_direction.r

### **PDaS line mapping & attribution**

**Drawing, splitting, attributing, flagging lines**

1. Received lines from Shuyao 
2. ./segment_pdas_s24_assetbases_VM/Segment_PDAS_S24/Segment_PDaS_Final/Dataflow Folder/2-R Script Chunker\3 - Splitter.r
3. ./segment_pdas_s24_assetbases_VM/Segment_PDAS_S24/Segment_S24_Final/Dataflow Folder/2-R Script Chunker\3 - Splitter.r
4. ./assign-attributes/predict d_m for inferred pdas.r
5. ./assign-attributes/predict d_m for inferred s24_no_fid.r
6. ./flag_overlap/flag_overlap.r (responsible for the pipe id) 
7. ./assign-attributes/result_statistics.r

